-- GenI surface realiser
-- Copyright (C) 2005 Carlos Areces and Eric Kow
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.GraphvizShowPolarity
where

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe ( catMaybes )
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

import qualified Data.Text.Lazy as T
import NLP.GenI.Btypes(showSem)
import NLP.GenI.General(showInterval)
import NLP.GenI.Polarity(PolAut, PolState(PolSt), NFA(states, transitions), finalSt)
import NLP.GenI.Graphviz(GraphvizShow(..), gvUnlines)
import NLP.GenI.Tags(idname)

instance GraphvizShow PolAut where
  -- we want a directed graph (arrows)
  graphvizShowGraph aut =
     DotGraph False True Nothing $ DotStmts
         [ GraphAttrs [RankDir FromLeft, RankSep [0.02], Pack (PackMargin 1)]
         , NodeAttrs [FontSize 10]
         , EdgeAttrs [FontSize 10]
         ]
         (graphvizShowAsSubgraph "aut" aut)
         [] -- all nodes are in the subgraph
         []

  --
  graphvizShowAsSubgraph prefix aut =
    [ DotSG False Nothing
            $ DotStmts [ NodeAttrs [ Shape Ellipse, Peripheries 1 ] ]
                       []
                       (zipWith (gvShowState fin) ids st)
                       (concat $ zipWith (gvShowTrans aut stmap) ids st)
    ]
    where
       st  = (concat.states) aut
       fin = finalSt aut
       ids = map (\x -> prefix `T.append` T.pack (show x)) ([0..] :: [Int])
       -- map which permits us to assign an id to a state
       stmap = Map.fromList $ zip st ids

gvShowState :: [PolState] -> T.Text -> PolState -> DotNode T.Text
gvShowState fin stId st =
  DotNode stId $ decorate [ Label . StrLabel . showSt $ st ]
  where
   showSt (PolSt _ ex po) =
          gvUnlines . catMaybes $
            [ Nothing -- Just (snd3 pr)
            , if null ex then Nothing else Just (T.pack (showSem ex))
            , Just . T.pack . intercalate "," $ map showInterval po
            ]
   decorate = if st `elem` fin
                 then (Peripheries 2 :)
                 else id

gvShowTrans :: PolAut -> Map.Map PolState T.Text
               -> T.Text -> PolState -> [DotEdge T.Text]
gvShowTrans aut stmap idFrom st =
  let -- outgoing transition labels from st
      trans = Map.findWithDefault Map.empty st $ transitions aut
      -- returns the graphviz dot command to draw a labeled transition
      drawTrans (stTo,x) = case Map.lookup stTo stmap of
                             Nothing   -> drawTrans' ("id_error_" `T.append` (T.pack (sem_ stTo))) x
                             Just idTo -> drawTrans' idTo x
                           where sem_ (PolSt i _ _) = show i
                                 --showSem (PolSt (_,pred,_) _ _) = pred
      drawTrans' idTo x = DotEdge idFrom idTo [Label (drawLabel x)]
      drawLabel labels  = StrLabel . gvUnlines $ labs
        where
          lablen  = length labels
          maxlabs = 6
          excess = T.pack $ "...and " ++ show (lablen - maxlabs) ++ " more"
          --
          labstrs = map (maybe "EMPTY" (T.pack . idname)) labels
          labs = if lablen > maxlabs
                 then take maxlabs labstrs ++ [ excess ]
                 else labstrs
  in map drawTrans (Map.toList trans)
