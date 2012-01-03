--  GenI surface realiser
--  Copyright (C) 2005 Carlos Areces and Eric Kow
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 2
--  of the License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

{-
We need to be able to dump some of GenI's data structures into a simple
text format we call GeniHand.

There are at least two uses for this, one is that it allows us to
interrupt the debugging process, dump everything to file, muck around
with the trees and then pick up where we left off.

The other use is to make large grammars faster to load.  We don't actually do
this anymore, mind you, but it's nice to have the option.  The idea is to take
a massive XML grammar, parse it to a set of TagElems and then write these back
in the lighter syntax.  It's not that XML is inherently less efficient to parse
than the handwritten syntax, just that writing an efficient parser for XML
based format is more annoying, so I stuck with HaXml to make my life easy.
Unfortunately, HaXml seems to have some kind of space leak.
-}

-- This module provides specialised functions for visualising tree data.
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module NLP.GenI.GeniShow
where

import Data.Tree
import Data.List(intersperse)
import qualified Data.Map as Map
import qualified Data.Text as T

import NLP.GenI.Semantics ( isInternalHandle, SemInput, Literal(..) )
import NLP.GenI.Tags
 ( TagElem, idname,
   tsemantics, ttree, tinterface, ttype, ttreename,
 )
import NLP.GenI.Btypes (GeniVal(..), AvPair(..), Ptype(..),
               Ttree(params, pidname, pfamily, pinterface, ptype, tree, psemantics, ptrace),
               GNode(..), GType(..),
               TestCase(..),
               )

class GeniShow a where
  geniShow :: a -> String

instance GeniShow Ptype where
 geniShow Initial  = "initial"
 geniShow Auxiliar = "auxiliary"
 geniShow _        = ""

instance GeniShow (AvPair GeniVal) where
 geniShow (AvPair a v) = T.unpack a ++ ":" ++ geniShow v

instance GeniShow GeniVal where
 geniShow x = show  x

instance GeniShow Literal where
 geniShow (Literal h p l) =
   showh ++ geniShow p ++ "(" ++ unwords (map geniShow l) ++ ")"
   where
    hideh g = case gConstraints g of
                Just [c] -> isInternalHandle c
                _        -> False
    showh = if hideh h then "" else geniShow h ++ ":"

instance GeniShow (GNode GeniVal) where
 geniShow x =
  let gaconstrstr = case (gaconstr x, gtype x) of
                    (True, Other) -> "aconstr:noadj"
                    _             ->  ""
      gtypestr n = case (gtype n) of
                     Subs -> "type:subst"
                     Foot -> "type:foot"
                     Lex  -> if ganchor n && (null.glexeme) n
                             then "type:anchor" else "type:lex"
                     _    -> ""
      glexstr n =
        if null ls then ""
        else concat $ intersperse "|" $ map quote ls
        where quote s = "\"" ++ s ++ "\""
              ls = glexeme n
      tbFeats n = (geniShow $ gup n) ++ "!" ++ (geniShow $ gdown n)
  in unwords $ filter (not.null) $ [ gnname x, gaconstrstr, gtypestr x, glexstr x, tbFeats x ]

instance (GeniShow a) => GeniShow [a] where
 geniShow = squares . unwords . (map geniShow)

instance (GeniShow a) => GeniShow (Tree a) where
 geniShow t =
  let treestr i (Node a l) =
        spaces i ++ geniShow a ++
        case (l,i) of
        ([], 0)  -> "{}"
        ([], _)  -> ""
        (_, _)   -> "{\n" ++ (unlines $ map next l) ++ spaces i ++ "}"
        where next = treestr (i+1)
      --
      spaces i = take i $ repeat ' '
  in treestr 0 t

instance GeniShow TagElem where
 geniShow te =
  "\n% ------------------------- " ++ idname te
  ++ "\n" ++ (ttreename te) ++ ":" ++ (idname te)
  ++ " "  ++ (geniShow.tinterface $ te)
  ++ " "  ++ (geniShow.ttype $ te)
  ++ "\n" ++ (geniShow.ttree $ te)
  ++ "\n" ++ geniShowKeyword "semantics" "" ++ (geniShow.tsemantics $ te)

instance (GeniShow a) => GeniShow (Ttree a) where
 geniShow tt =
  "\n% ------------------------- " ++ pidname tt
  ++ "\n" ++ (pfamily tt) ++ ":" ++ (pidname tt)
  ++ " "  ++ (parens $    (unwords $ map geniShow $ params tt)
                       ++ " ! "
                       ++ (unwords $ map geniShow $ pinterface tt))
  ++ " "  ++ (geniShow.ptype $ tt)
  ++ "\n" ++ (geniShow.tree $ tt)
  ++ (case psemantics tt of
      Nothing   -> ""
      Just psem -> "\n" ++ geniShowKeyword "semantics" (geniShow psem))
  ++ "\n" ++ geniShowKeyword "trace" (squares.unwords.ptrace $ tt)

instance GeniShow TestCase where
 geniShow (TestCase { tcName = name
                    , tcExpected = sentences
                    , tcOutputs = outputs
                    , tcSemString = semStr
                    , tcSem = sem }) =
  unlines $ [ name, semS ]
            ++ map (geniShowKeyword "sentence" . squares) sentences
            ++ (concat.prettify.map outStuff $ outputs)
  where
   semS     = if null semStr then geniShowSemInput sem "" else semStr
   prettify = if all (Map.null . snd) outputs then id else map ("":)
   gshowTrace ((k1,k2),ts) =
     geniShowKeyword "trace" . squares . showString (k1 ++ " " ++  k2 ++ " ! " ++ unwords ts) $ ""
   outStuff (o,ds) =
     [ geniShowKeyword "output"   . squares $ o ]
     ++ (map gshowTrace $ Map.toList ds)


parens, squares :: String -> String
parens s  = "(" ++ s ++ ")"
squares s = "[" ++ s ++ "]"

geniShowKeyword :: String -> ShowS
geniShowKeyword k = showString k . showChar ':'

geniShowSemInput :: SemInput -> ShowS
geniShowSemInput (sem,icons,lcons) =
  let withConstraints lit =
        case concat [ cs | (p,cs) <- lcons, p == lit ] of
        [] -> geniShow lit
        cs -> geniShow lit ++ (squares . unwords $ cs)
      semStuff = geniShowKeyword "semantics" . squares
               . (showString . unwords . map withConstraints $ sem)
      idxStuff = geniShowKeyword "idxconstraints"
               . (showString . geniShow $ icons) . squares
 in semStuff .  (if null icons then id else showChar '\n' . idxStuff)
