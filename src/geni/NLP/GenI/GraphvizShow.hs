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
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Outputting core GenI data to graphviz.
module NLP.GenI.GraphvizShow
where

import Data.List ( nub )
import Data.List.Split (wordsBy)
import Data.Maybe(listToMaybe, maybeToList)

import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy as T

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
                (prefix `T.append` "DerivedTree0")
                (hfn `fmap` ttree te)
    ]

 graphvizLabel _ te =
  -- we display the tree semantics as the graph label
  let treename   = "name: "      `T.append` T.pack (idname te)
      semlist    = "semantics: " `T.append` gvShowSem (tsemantics te)
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

gvShowSem :: Sem -> T.Text
gvShowSem = gvUnlines . map T.pack . map unwords . clumpBy length 72 . words . showSem

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
       summary = if T.null extra
                 then FieldLabel stub
                 else FlipFields [ FieldLabel stub, FieldLabel extra ]
       body = Label $
              if not detailed then (StrLabel (graphvizShow_ gn))
              else RecordLabel [ FlipFields $
                                   [ summary
                                   , FieldLabel . showFs $ gup gn
                                   ] ++ (maybeFs (gdown gn))
                   ]
        where showFs = gvUnlines . map graphvizShow_
              maybeFs fs = if null fs then [] else [FieldLabel (showFs fs)]
   in DotNode prefix (body : shapeParams ++ colorParams)

instance GraphvizShowString () (GNode GeniVal) where
  graphvizShow () gn =
    let stub  = showGnStub gn
        extra = showGnDecorations gn
    in stub `T.append` extra

instance GraphvizShowString () (AvPair GeniVal) where
  graphvizShow () (AvPair a v) = T.pack (a ++ ":") `T.append` graphvizShow_ v

instance GraphvizShowString () GeniVal where
  graphvizShow () (GeniVal Nothing Nothing)    = "?_"
  graphvizShow () (GeniVal Nothing (Just cs))  = T.intercalate "!" (map T.fromChunks [cs])
  graphvizShow () (GeniVal (Just l) Nothing)   = '?' `T.cons` (T.pack l)
  graphvizShow () (GeniVal (Just l) (Just cs)) = '?' `T.cons` (T.concat [T.pack l, "/", T.intercalate "!" (map T.fromChunks [cs])])

showGnDecorations :: GNode GeniVal -> T.Text
showGnDecorations gn =
  case gtype gn of
  Subs -> "↓"
  Foot -> "*"
  _    -> if gaconstr gn then "ᴺᴬ"   else ""

showGnStub :: GNode GeniVal -> T.Text
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
     idx  = tackOn "." idxT idxB
     --
     lexeme  = T.intercalate "!" (map T.pack (glexeme gn))
 in T.intercalate ":" $ filter (not . T.null) [ cat, idx, lexeme ]

getGnVal :: (GNode GeniVal -> Flist GeniVal) -> String -> GNode GeniVal -> Maybe GeniVal
getGnVal getFeat attr gn =
  listToMaybe [ v | AvPair a v <- getFeat gn, a == attr ]

-- | @x `tackOn p` y@` is @T.concat [x, p, y]@ if @y@ is not null
--   otherwise is just x
tackOn :: T.Text -> T.Text -> T.Text -> T.Text
tackOn p x y = if T.null y then x else T.concat [ x, p, y ]

graphvizShow_ :: (GraphvizShowString () a) => a -> T.Text
graphvizShow_ = graphvizShow ()

-- ----------------------------------------------------------------------
-- Derivation tree
-- ----------------------------------------------------------------------

graphvizShowDerivation :: TagDerivation -> [DotSubGraph T.Text]
graphvizShowDerivation = maybeToList . derivationToGv

derivationToGv :: TagDerivation -> Maybe (DotSubGraph T.Text)
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
      DotNode (gvDerivationLab n) [ Label . StrLabel $ label n ]
    mkEdge (DerivationStep substadj child parent _) =
      DotEdge (gvDerivationLab parent) (gvDerivationLab child) xs
      where xs = if substadj == 'a' then [Style [SItem Dashed []]] else []
    label n = case wordsBy (== ':') n of
              name:fam:tree:_ -> T.pack $ name ++ ":" ++ fam ++ "\n" ++ tree
              _               -> T.pack n `T.append` " (geni/gv ERROR)"

gvDerivationLab :: String -> T.Text
gvDerivationLab xs = T.pack ("Derivation" ++ gvMunge xs)

-- | Node names can't have hyphens in them and newlines within the node
--   labels should be represented literally as @\\n@.
gvMunge :: String -> String
gvMunge = map dot2x . filter (/= ':') . filter (/= '-')

dot2x :: Char -> Char
dot2x '.' = 'x'
dot2x c   = c
