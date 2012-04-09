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

-- This module provides specialised functions for visualising tree data.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.GenI.GeniShow
where

import Data.Text ( Text )
import qualified Data.Text as T

-- | GenI format; should round-trip with 'NLP.GenI.Parser' by rights
--
--   Minimal definition, either one of 'geniShow' or 'geniShowText'
class GeniShow a where
    geniShow :: a -> String
    geniShow = T.unpack . geniShowText

    geniShowText :: a -> Text
    geniShowText = T.pack . geniShow

{-
instance GeniShow Ptype where
 geniShow Initial  = "initial"
 geniShow Auxiliar = "auxiliary"

instance GeniShow GeniVal where
   geniShow = prettyStr

instance GeniShow Literal where
   geniShow = prettyStr

-- TODO: does not support semantic polarities yet
instance GeniShow ILexEntry where
 geniShow l = intercalate "\n"
    [ unwords [ geniShow . mkGConst . fmap T.pack $ iword l
              , ifamname l
              , parens . unwords $ concat [ map geniShow (iparams l), ["!"], map geniShow (iinterface l) ]
              ]
    , geniShowKeyword "equations" $ geniShow (iequations l)
    , geniShowKeyword "filters"   $ geniShow (ifilters l)
    , geniShowKeyword "semantics" $ geniShow (isemantics l)
    ]

geniShowSmallList :: GeniShow a => [a] -> String
geniShowSmallList = squares . unwords . (map geniShow)

instance GeniShow [Literal] where
 geniShow = geniShowSmallList

instance GeniShow (AvPair v) => GeniShow [AvPair v] where
 geniShow = geniShowSmallList

instance GeniShow [ILexEntry] where
 geniShow = intercalate "\n\n" . map geniShow

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
   semS     = if null semStr then geniShow sem else semStr
   prettify = if all (Map.null . snd) outputs then id else map ("":)
   gshowTrace ((k1,k2),ts) =
     geniShowKeyword "trace" . squares . showString (k1 ++ " " ++  k2 ++ " ! " ++ unwords ts) $ ""
   outStuff (o,ds) =
     [ geniShowKeyword "output"   . squares $ o ]
     ++ (map gshowTrace $ Map.toList ds)
-}

{-
parens, squares :: String -> String
parens s  = "(" ++ s ++ ")"
squares s = "[" ++ s ++ "]"
-}

geniKeyword :: Text -> Text  -> Text
geniKeyword k t = k `T.append` ":" `T.append` t
