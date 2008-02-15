% GenI surface realiser
% Copyright (C) 2005 Carlos Areces and Eric Kow
%
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation; either version 2
% of the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
module NLP.GenI.GraphvizShowPolarity
where

import Data.List (intersperse)
import qualified Data.Map as Map

import NLP.GenI.Btypes(showSem)
import NLP.GenI.General(showInterval, isEmptyIntersect)
import NLP.GenI.Polarity(PolAut, PolState(PolSt), NFA(states, transitions), finalSt)
import NLP.GenI.Graphviz(GraphvizShow(..), gvUnlines, gvNewline, gvNode, gvEdge)
import NLP.GenI.Tags(idname)
\end{code}

\begin{code}
instance GraphvizShow () PolAut where
  -- we want a directed graph (arrows)
  graphvizShowGraph f aut =
     "digraph aut {\n"
     ++ "rankdir=LR\n"
     ++ "ranksep = 0.02\n"
     ++ "pack=1\n"
     ++ "edge [ fontsize=10 ]\n"
     ++ "node [ fontsize=10 ]\n"
     ++ graphvizShowAsSubgraph f "aut" aut
     ++ "}"

  --
  graphvizShowAsSubgraph _ prefix aut =
   let st  = (concat.states) aut
       ids = map (\x -> prefix ++ show x) ([0..] :: [Int])
       -- map which permits us to assign an id to a state
       stmap = Map.fromList $ zip st ids
   in --
      gvShowFinal aut stmap
      -- any other state should be an ellipse
      ++ "node [ shape = ellipse, peripheries = 1 ]\n"
      -- draw the states and transitions
      ++ (concat $ zipWith gvShowState ids st)
      ++ (concat $ zipWith (gvShowTrans aut stmap) ids st )
\end{code}

\begin{code}
gvShowState :: String -> PolState -> String
gvShowState stId st =
  -- note that we pass the label param explicitly to allow for null label
  gvNode stId "" [ ("label", showSt st) ]
  where showSt (PolSt pr ex po) = showPr pr ++ showEx ex ++ showPo po
        showPr _ = "" -- (_,pr,_) = pr ++ gvNewline
        showPo po = concat $ intersperse "," $ map showInterval po
        showEx ex = if null ex then "" else showSem ex ++ gvNewline
\end{code}

Specify that the final states are drawn with a double circle

\begin{code}
gvShowFinal :: PolAut -> Map.Map PolState String -> String
gvShowFinal aut stmap =
  if isEmptyIntersect (concat $ states aut) fin
  then ""
  else "node [ peripheries = 2 ]; "
  ++ concatMap (\x -> " " ++ lookupId x) fin
  ++ "\n"
  where fin = finalSt aut
        lookupId x = Map.findWithDefault "error_final" x stmap
\end{code}

Each transition is displayed with the name of the tree.  If there is more
than one transition to the same state, they are displayed on a single
label.

\begin{code}
gvShowTrans :: PolAut -> Map.Map PolState String
               -> String -> PolState -> String
gvShowTrans aut stmap idFrom st =
  let -- outgoing transition labels from st
      trans = Map.findWithDefault Map.empty st $ transitions aut
      -- returns the graphviz dot command to draw a labeled transition
      drawTrans (stTo,x) = case Map.lookup stTo stmap of
                             Nothing   -> drawTrans' ("id_error_" ++ (sem_ stTo)) x
                             Just idTo -> drawTrans' idTo x
                           where sem_ (PolSt i _ _) = show i
                                 --showSem (PolSt (_,pred,_) _ _) = pred
      drawTrans' idTo x = gvEdge idFrom idTo (drawLabel x) []
      drawLabel labels  = gvUnlines labs
        where
          lablen  = length labels
          maxlabs = 6
          excess = "...and " ++ (show $ lablen - maxlabs) ++ " more"
          --
          labstrs = map fn labels
          fn Nothing  = "EMPTY"
          fn (Just x) = idname x
          --
          labs = if lablen > maxlabs
                 then take maxlabs labstrs ++ [ excess ]
                 else labstrs
  in unlines $ map drawTrans $ Map.toList trans
\end{code}

%gvShowTransPred te =
%  let p = tpredictors te
%      charge fv = case () of _ | c == -1   -> "-"
%                               | c ==  1   -> "+"
%                               | c  >  0   -> "+" ++ (show c)
%                               | otherwise -> (show c)
%                  where c = lookupWithDefaultFM p 0 fv
%      showfv (f,v) = charge (f,v) ++ f
%                   ++ (if (null v) then "" else ":" ++ v)
%  in map showfv $ Map.keys p

