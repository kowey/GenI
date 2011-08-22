--  GenI surface realiser
--  Copyright (C) 2009 Eric Kow
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

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Outputting core GenI data to graphviz.
module NLP.GenI.GraphvizShow
where

import Data.List(intersperse,nub)
import Data.List.Split (wordsBy)
import Data.Maybe(listToMaybe, maybeToList)

import Data.GraphViz

import NLP.GenI.Tags
 ( TagElem, TagDerivation, idname,
   tsemantics, ttree,
   DerivationStep(..),
 )
import NLP.GenI.Btypes (AvPair(..),
               GNode(..), GType(..), Flist,
               showSem,
               )
import NLP.GenI.General ( clumpBy )
import NLP.GenI.GeniVal (GeniVal(..),isConst)
import NLP.GenI.Graphviz
  ( GraphvizShow(graphvizShowAsSubgraph, graphvizLabel, graphvizParams)
  , GraphvizShowNode(graphvizShowNode)
  , GraphvizShowString(graphvizShow)
  , gvUnlines, gvShowTree
  )
import NLP.GenI.Semantics ( Sem )

-- ----------------------------------------------------------------------
-- For GraphViz
-- ----------------------------------------------------------------------

type GvHighlighter a = a -> (a, Maybe Color)

nullHighlighter :: GvHighlighter (GNode GeniVal)
nullHighlighter a = (a,Nothing)

instance GraphvizShow Bool TagElem where
 graphvizShowAsSubgraph sf = graphvizShowAsSubgraph (sf, nullHighlighter)
 graphvizLabel  sf = graphvizLabel (sf, nullHighlighter )
 graphvizParams sf = graphvizParams (sf, nullHighlighter)


instance GraphvizShow (Bool, GvHighlighter (GNode GeniVal)) TagElem where
 graphvizShowAsSubgraph (sf,hfn) prefix te =
    [gvShowTree sf
                (prefix ++ "DerivedTree0")
                (hfn `fmap` ttree te)
    ]

 graphvizLabel _ te =
  -- we display the tree semantics as the graph label
  let treename   = "name: " ++ (idname te)
      semlist    = "semantics: " ++ gvShowSem (tsemantics te)
  in gvUnlines [ treename, semlist ]

 graphvizParams _ _ =
  [ GraphAttrs [ FontSize 10
               , RankSep [0.3]
               ]
  , NodeAttrs  [ FontSize 10
               ]
  , EdgeAttrs  [ FontSize 10
               , ArrowHead normal
               ]
  ]

gvShowSem :: Sem -> String
gvShowSem = gvUnlines . map unwords . clumpBy length 32 . words . showSem 

-- ----------------------------------------------------------------------
-- Helper functions for the TagElem GraphvizShow instance
-- ----------------------------------------------------------------------

instance GraphvizShowNode (Bool) (GNode GeniVal, Maybe Color) where
 -- compact -> (node, mcolour) -> String
 graphvizShowNode detailed prefix (gn, mcolour) =
   let -- attributes
       filledParam         = Style [SItem Filled []]
       fillcolorParam      = FillColor (X11Color LemonChiffon)
       shapeRecordParam    = Shape Record
       shapePlaintextParam = Shape PlainText
       --
       colorParams = case mcolour of
                     Nothing -> []
                     Just c  -> [ FontColor c ]
       shapeParams = if detailed
                     then [ shapeRecordParam, filledParam, fillcolorParam ]
                     else [ shapePlaintextParam ]
       -- content
       stub  = showGnStub gn
       extra = showGnDecorations gn
       summary = if null extra
                 then FieldLabel stub
                 else FlipFields [ FieldLabel stub, FieldLabel extra ]
       body = Label $
              if not detailed then (StrLabel (graphvizShow_ gn))
              else RecordLabel [ FlipFields $
                                   [ summary
                                   , FieldLabel . showFs $ gup gn
                                   ] ++ (maybeFs (gdown gn))
                   ]
        where showFs = unlines . map graphvizShow_
              maybeFs fs = if null fs then [] else [FieldLabel (showFs fs)]
   in DotNode prefix (body : shapeParams ++ colorParams)

instance GraphvizShowString () (GNode GeniVal) where
  graphvizShow () gn =
    let stub  = showGnStub gn
        extra = showGnDecorations gn
    in stub ++ extra

instance GraphvizShowString () (AvPair GeniVal) where
  graphvizShow () (AvPair a v) = a ++ ":" ++ graphvizShow_ v

instance GraphvizShowString () GeniVal where
  graphvizShow () (GeniVal Nothing Nothing)    = "?_"
  graphvizShow () (GeniVal Nothing (Just cs))  = concat (intersperse "!" cs)
  graphvizShow () (GeniVal (Just l) Nothing)   = '?':l
  graphvizShow () (GeniVal (Just l) (Just cs)) = '?':concat (l : "/" : intersperse "!" cs)

showGnDecorations :: GNode GeniVal -> String
showGnDecorations gn =
  case gtype gn of
  Subs -> "↓"
  Foot -> "*"
  _    -> if gaconstr gn then "ᴺᴬ"   else ""

showGnStub :: GNode GeniVal -> String
showGnStub gn =
 let cat = case getGnVal gup "cat" gn of
           Nothing -> ""
           Just v  -> graphvizShow_ v
     --
     getIdx f =
       case getGnVal f "idx" gn of
       Nothing -> ""
       Just v  -> if isConst v then graphvizShow_ v else ""
     idxT = getIdx gup
     idxB = getIdx gdown
     idx  = idxT ++ (maybeShow_ "." idxB)
     --
     lexeme  = concat $ intersperse "!" $ glexeme gn
 in concat $ intersperse ":" $ filter (not.null) [ cat, idx, lexeme ]

getGnVal :: (GNode GeniVal -> Flist GeniVal) -> String -> GNode GeniVal -> Maybe GeniVal
getGnVal getFeat attr gn =
  listToMaybe [ v | AvPair a v <- getFeat gn, a == attr ]

-- | Apply fn to s if s is not null
maybeShow :: ([a] -> String) -> [a] -> String
maybeShow fn s = if null s then "" else fn s
-- | Prefix a string if it is not null
maybeShow_ :: String -> String -> String
maybeShow_ prefix s = maybeShow (prefix++) s

graphvizShow_ :: (GraphvizShowString () a) => a -> String
graphvizShow_ = graphvizShow ()

-- ----------------------------------------------------------------------
-- Derivation tree
-- ----------------------------------------------------------------------

graphvizShowDerivation :: TagDerivation -> [DotSubGraph String]
graphvizShowDerivation = maybeToList . derivationToGv

derivationToGv :: TagDerivation -> Maybe (DotSubGraph String)
derivationToGv deriv =
 if null histNodes
    then Nothing
    else Just $ DotSG False Nothing $ DotStmts atts [] nodes edges
  where
    atts = [ NodeAttrs [ Shape PlainText ]
           , EdgeAttrs [ ArrowHead noArrow ]
           ]
    nodes = map mkNode histNodes
    edges = map mkEdge deriv
    --
    histNodes = reverse $ nub $ concatMap (\ (DerivationStep _ c p _) -> [c,p]) deriv
    mkNode n  =
      DotNode (gvDerivationLab n) [ Label . StrLabel . label $ n ]
    mkEdge (DerivationStep substadj child parent _) =
      DotEdge (gvDerivationLab parent) (gvDerivationLab child) True xs
      where xs = if substadj == 'a' then [Style [SItem Dashed []]] else []
    label n = case wordsBy (== ':') n of
              name:fam:tree:_ -> name ++ ":" ++ fam ++ "\n" ++ tree
              _               -> n ++ " (geni/gv ERROR)"

gvDerivationLab :: String -> String
gvDerivationLab xs = "Derivation" ++ gvMunge xs

-- | Node names can't have hyphens in them and newlines within the node
--   labels should be represented literally as @\\n@.
gvMunge :: String -> String
gvMunge = map dot2x . filter (/= ':') . filter (/= '-')

dot2x :: Char -> Char
dot2x '.' = 'x'
dot2x c   = c
