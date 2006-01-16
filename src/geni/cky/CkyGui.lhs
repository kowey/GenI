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

import Control.Monad.State (runState)
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
  ( ckyBuilder, setup, BuilderStatus, ChartItem(..), genconfig 
  , bitVectorToSem
  , theResults, theAgenda, theChart, theTrash
  , SentenceAut)
import Configuration ( Params(..), polarised, chartsharing )

import Geni 
  ( ProgState(..), ProgStateRef
  , initGeni, runGeni
  , showRealisations )
import General ( snd3, listRepNode )
import Graphviz ( GraphvizShow(..), gvNode, gvEdge, gvSubgraph, gvUnlines )
import GuiHelper

import Polarity
import Tags 
  ( idname, emptyTE 
  , tdiagnostic, tsemantics, thighlight, ttree
  , TagElem, derivation)
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
    debugTab <- debuggerTab nb config (initStuff { B.inCands = combos }) "cky"
    let genTabs = [ tab "session" debugTab ]
    --
    set f [ layout := container p $ column 0 [ tabs nb (basicTabs ++ genTabs) ]
          , clientSize := sz 700 600 ]
    return ()
\end{code}
  
The generation could conceivably be broken into multiple generation
tasks, so we create a separate tab for each task.

\begin{code}
debuggerTab :: (Window a) -> Params -> B.Input -> String -> IO Layout 
debuggerTab f config input cachedir = 
 do let initBuilder = B.init  ckyBuilder
        nextStep    = B.step  ckyBuilder
        manySteps   = B.stepAll ckyBuilder
        genstats    = B.stats ckyBuilder
        --
    let initSt  = initBuilder input (config {usetrash=True})
        (items,labels) = showGenState initSt 
    -- widgets
    p <- panel f []      
    gvRef <- newGvRef False labels "debugger session" 
    setGvDrawables gvRef items 
    (lay,updaterFn) <- graphvizGui p cachedir gvRef 
    detailsChk <- checkBox p [ text := "Show features"
                             , checked := False ]
    restartBt   <- button p [text := "Start over"]
    nextBt   <- button p [text := "Leap by..."]
    leapVal  <- entry p [ text := "1", clientSize := sz 30 25 ]
    finishBt <- button p [text := "Continue"]
    statsTxt <- staticText p []
    -- commands
    let updateStatsTxt gs = set statsTxt [ text :~ (\_ -> txtStats gs) ]
        txtStats   gs =  "itr " ++ (show $ B.geniter gs) ++ " " 
                      ++ "chart sz: " ++ (show $ B.szchart gs) 
                      ++ "\ncomparisons: " ++ (show $ B.numcompar gs)
    let onDetailsChk = 
          do isDetailed <- get detailsChk checked 
             setGvParams gvRef isDetailed
             updaterFn
    let genStep _ st = snd $ runState nextStep st
    let showNext s = 
          do leapTxt <- get leapVal text
             let leapInt = read leapTxt
                 s2 = foldr genStep s [1..leapInt]
             setGvDrawables2 gvRef (showGenState s2)
             setGvSel gvRef 1
             updaterFn
             updateStatsTxt (genstats s2)
             set nextBt [ on command :~ (\_ -> showNext s2) ]
    let showLast = 
          do -- redo generation from scratch
             let s = snd $ runState manySteps initSt 
             setGvDrawables2 gvRef (showGenState s)
             updaterFn
             updateStatsTxt (genstats s)
    let showReset = 
          do let st  = initSt 
             set nextBt   [ on command  := showNext st ]
             updateStatsTxt (B.initGstats)
             setGvDrawables2 gvRef (showGenState st)
             setGvSel gvRef 1
             updaterFn
    -- handlers
    set detailsChk [ on command := onDetailsChk ] 
    set finishBt [ on command := showLast ]
    set restartBt [ on command := showReset ]
    showReset
    -- pack it all in      
    let cmdBar = hfloatRight $ row 5 [ dynamic $ widget detailsChk
                   , widget restartBt
                   , widget nextBt 
                   , widget leapVal, label " step(s)"
                   , widget finishBt 
                   ]
        lay2   = fill $ container p $ column 5 [ lay, row 5 
                   [ hfill $ widget statsTxt, cmdBar ] ] 
    return lay2 
\end{code}

\paragraph{showGenState} converts the generator state into a list
of trees and labels the way graphvizGui likes it.

\begin{code}
showGenState :: BuilderStatus -> ([Maybe ChartItem],[String])
showGenState st = 
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
      labelFn i =  (idname $ ciSourceTree i) ++ " " ++ (gnname $ ciNode i) 
                ++ (showPaths i) 
  in unzip $ agenda ++ chart ++ results ++ trash 
\end{code}

\section{Helper code}

\begin{code}
instance GraphvizShow Bool ChartItem where
  graphvizLabel  f = graphvizLabel  f . toTagElem 
  graphvizShowAsSubgraph f prefix ci = 
   let gvTree = graphvizShowAsSubgraph f  (prefix ++ "tree")  $ toTagElem ci
       gvAut1 = graphvizShowAsSubgraph () (prefix ++ "aut1")  $ ciAut_beforeHole ci
       -- FIXME: maybe it would be nice to connect these two automaton using a specially
       -- marked label - we could simulate this by joining the two automata and naming
       -- the bridge specially
       gvAut2 = graphvizShowAsSubgraph () (prefix ++ "aut2")  $ ciAut_afterHole ci
   -- FIXME: will have to make this configurable, maybe, show aut, show tree? radio button?
   in    (unlines $ graphvizParams f $ ciSourceTree ci)
      ++ gvSubgraph gvTree
      ++ (unlines $ graphvizParams () $ ciAut_beforeHole ci)
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
instance GraphvizShow () SentenceAut where
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

type SentenceAutState = String

gvShowState :: String -> SentenceAutState -> String
gvShowState stId st = gvNode stId st []

gvShowTrans :: SentenceAut -> Map.Map SentenceAutState String
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
                             Nothing   -> drawTrans' ("id_error_" ++ stTo) x 
                             Just idTo -> drawTrans' idTo x
      drawTrans' idTo x = gvEdge idFrom idTo (drawLabel x) []
      drawLabel labels  = gvUnlines $ catMaybes labels 
  in concatMap drawTrans $ Map.toList invFM
\end{code}
