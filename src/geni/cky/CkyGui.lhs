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

\chapter{Graphical User Interface for CKY} 

\begin{code}
module CkyGui where
\end{code}

\ignore{
\begin{code}
import Graphics.UI.WX
import Graphics.UI.WXCore

import qualified Control.Monad as Monad 

import Data.IORef
import Data.List (find, nub, (\\))
import qualified Data.Map as Map 
import Data.Maybe (isJust, catMaybes)
import Data.Tree 

import Automaton ( NFA(states, transitions), lookupTrans )
import qualified Builder    as B
import qualified BuilderGui as BG 
import Btypes (gnname, showLexeme, iword, isemantics)

import CkyBuilder 
  ( ckyBuilder, setup, BuilderStatus, ChartItem(..),
  , bitVectorToSem
  , theResults, theAgenda, theChart, theTrash
  )
import Configuration ( Params(..), polarised )

import Geni 
  ( ProgState(..), ProgStateRef
  , initGeni )
import General ( listRepNode )
import Graphviz ( GraphvizShow(..), gvNode, gvEdge, gvSubgraph, gvUnlines )
import GuiHelper 
  ( candidateGui, messageGui, polarityGui, toSentence
  , debuggerPanel, DebuggerItemBar
  , setGvParams) 

import Polarity
import Tags 
  ( tdiagnostic, tsemantics, thighlight, ttree, TagElem )
\end{code}
}

% --------------------------------------------------------------------
\section{Interface}
% --------------------------------------------------------------------

\begin{code}
ckyGui = BG.BuilderGui {
      BG.generateGui = generateGui 
    , BG.debugGui = debugGui }

generateGui :: ProgStateRef -> IO ()
generateGui _ = 
 do f <- frame []
    messageGui f "FIXME: Not implemented yet! Use Debug instead"
    return ()
\end{code}

% --------------------------------------------------------------------
\section{Debugger}
\label{sec:debugger_gui}
\label{fn:debugGui}
% --------------------------------------------------------------------

This creates an iteractive version of the generator that shows the
user the agenda, chart and results at various stages in the generation
process.  

\begin{code}
debugGui :: ProgStateRef -> IO ()
debugGui pstRef = 
 do pst <- readIORef pstRef
    let config = pa pst
    --
    f <- frame [ text := "GenI Debugger - CKY edition" 
               , fullRepaintOnResize := False 
               , clientSize := sz 300 300 ] 
    p    <- panel f []
    nb   <- notebook p []
    -- generation step 1
    initStuff <- initGeni pstRef
    let (tsem,_)   = B.inSemInput initStuff
        cand       = B.inCands initStuff 
        lexonly    = B.inLex initStuff 
    -- candidate selection tab
    let missedSem  = tsem \\ (nub $ concatMap tsemantics cand)
        -- we assume that for a tree to correspond to a lexical item,
        -- it must have the same semantics
        hasTree l = isJust $ find (\t -> tsemantics t == lsem) cand
          where lsem = isemantics l
        missedLex = [ showLexeme (iword l) | l <- lexonly, (not.hasTree) l ]
    canTab <- candidateGui pst nb cand missedSem missedLex
    -- generation step 2.A (run polarity stuff)
    let (combos, autstuff) = setup initStuff config
    -- automata tab
    let (auts, finalaut) = autstuff
    autTab <- if polarised config 
              then polarityGui nb auts finalaut
              else messageGui nb "polarities disabled"
    -- basic tabs 
    let basicTabs = tab "lexical selection" canTab :
                    (if polarised config then [tab "automata" autTab] else [])
    -- generation step 2.B (start the generator for each path)
    debugTab <- ckyDebuggerTab nb config (initStuff { B.inCands = combos }) "cky"
    let genTabs = [ tab "session" debugTab ]
    --
    set f [ layout := container p $ column 0 [ tabs nb (basicTabs ++ genTabs) ]
          , clientSize := sz 700 600 ]
    return ()
\end{code}
  
The generation could conceivably be broken into multiple generation
tasks, so we create a separate tab for each task.

\begin{code}
ckyDebuggerTab :: (Window a) -> Params -> B.Input -> String -> IO Layout 
ckyDebuggerTab = debuggerPanel ckyBuilder False stateToGv ckyItemBar
 where 
  stateToGv st = 
   let agenda  = section "AGENDA"  $ theAgenda  st
       trash   = section "TRASH"   $ theTrash   st
       chart   = section "CHART"   $ theChart   st
       results = section "RESULTS" $ theResults st
       --
       section n i = hd : (map tlFn i)
         where hd = (Nothing, "___" ++ n ++ "___")
               tlFn x = (Just x, labelFn x)
       showPaths = const ""
                   {- if (polarised $ genconfig st)
                      then (\t -> " (" ++ showPolPaths t ++ ")")
                      else const "" -}
       labelFn i = (toSentence $ ciSourceTree i) ++ " " ++ (gnname $ ciNode i) 
                 ++ (showPaths i) 
   in unzip $ agenda ++ chart ++ results ++ trash 
 
ckyItemBar :: DebuggerItemBar Bool ChartItem
ckyItemBar f gvRef updaterFn =
 do ib <- panel f []
    detailsChk <- checkBox ib [ text := "Show features"
                              , checked := False ]
    let onDetailsChk = 
         do isDetailed <- get detailsChk checked 
            setGvParams gvRef isDetailed
            updaterFn
    set detailsChk [ on command := onDetailsChk ] 
    return $ hfloatCentre $ container ib $ row 5 [ dynamic $ widget detailsChk ]
\end{code}

\section{Helper code}

\begin{code}
instance GraphvizShow Bool ChartItem where
  graphvizLabel  f = graphvizLabel  f . toTagElem 
  graphvizShowAsSubgraph f prefix ci = 
   let gvTree = graphvizShowAsSubgraph f  (prefix ++ "tree")  $ toTagElem ci
       gvAut1 = graphvizShowAsSubgraph () (prefix ++ "aut1")  $ ciAut_befHole ci
       -- FIXME: maybe it would be nice to connect these two automaton using a specially
       -- marked label - we could simulate this by joining the two automata and naming
       -- the bridge specially
       gvAut2 = graphvizShowAsSubgraph () (prefix ++ "aut2")  $ ciAut_aftHole ci
   -- FIXME: will have to make this configurable, maybe, show aut, show tree? radio button?
   in    (unlines $ graphvizParams f $ ciSourceTree ci)
      ++ gvSubgraph gvTree
      ++ (unlines $ graphvizParams () $ ciAut_befHole ci)
      ++ gvSubgraph gvAut1 ++ gvSubgraph gvAut2
      ++ (unlines $ graphvizParams f  $ ciSourceTree ci)

toTagElem :: ChartItem -> TagElem
toTagElem ci =
 let node = ciNode ci
     te   = ciSourceTree ci
     --
     updateTree t = (head.fst) $ listRepNode replaceFn filtFn [t]
      where filtFn (Node a _)    = (gnname a == gnname node)
            replaceFn (Node _ k) = Node node k
     --
 in te { ttree = (updateTree.ttree) te
       , tsemantics  = bitVectorToSem (ciSemBitMap ci) (ciSemantics ci) 
       , tdiagnostic = ciDiagnostic ci
       , thighlight  = [gnname node] }

-- FIXME: this is largely copy-and-pasted from Polarity.lhs 
-- it should be refactored later
instance GraphvizShow () B.SentenceAut where
  graphvizShowAsSubgraph f prefix aut =
   let st  = (concat.states) aut
       ids = map (\x -> prefix ++ show x) [0..]
       -- map which permits us to assign an id to a state
       stmap = Map.fromList $ zip st ids
   in --
      -- any state should be an ellipse
      "node [ shape = ellipse, peripheries = 1 ]\n"
      -- draw the states and transitions 
      ++ (concat $ zipWith gvShowState ids st) 
      ++ (concat $ zipWith (gvShowTrans aut stmap) ids st )

type SentenceAutState = Int 

gvShowState :: String -> SentenceAutState -> String
gvShowState stId st = gvNode stId (show st) []

gvShowTrans :: B.SentenceAut -> Map.Map SentenceAutState String
               -> String -> SentenceAutState -> String 
gvShowTrans aut stmap idFrom st = 
  let -- outgoing transition labels from st
      alpha = Map.keys $ Map.findWithDefault Map.empty st (transitions aut) 
      -- associate each st2 with a list of labels that transition to it
      inverter x fm = foldr fn fm (lookupTrans aut st x)
                      where fn    s f   = Map.insert s (xlist s f x) f
                            xlist s f x = x:(Map.findWithDefault [] s f)
      invFM = foldr inverter Map.empty alpha
      -- returns the graphviz dot command to draw a labeled transition
      drawTrans (stTo,x) = case Map.lookup stTo stmap of
                             Nothing   -> drawTrans' ("id_error_" ++ (show stTo)) x 
                             Just idTo -> drawTrans' idTo x
      drawTrans' idTo x = gvEdge idFrom idTo (drawLabel x) []
      drawLabel labels  = gvUnlines $ map fst $ catMaybes labels 
  in concatMap drawTrans $ Map.toList invFM
\end{code}
