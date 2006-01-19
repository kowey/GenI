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

\chapter{Graphical User Interface} 

\begin{code}
module SimpleGui where
\end{code}

\ignore{
\begin{code}
import Graphics.UI.WX
import Graphics.UI.WXCore

import qualified Control.Monad as Monad 

import Control.Monad.State (runState)
import Data.IORef
import Data.List (find, nub, (\\))
import Data.Maybe (isJust)

import Geni 
  ( ProgState(..), ProgStateRef
  , initGeni, runGeni
  , showRealisations )
import Btypes 
  (showLexeme,
   iword, isemantics)
import Tags (emptyTE,tsemantics,thighlight, 
             TagElem)

import Configuration ( Params(..), polarised, chartsharing )
import GuiHelper

import qualified Builder    as B
import qualified BuilderGui as BG 
import Polarity
import SimpleBuilder 
  ( simpleBuilder, setup, SimpleStatus, genconfig 
  , theResults, theAgenda, theAuxAgenda, theChart, theTrash)
\end{code}
}

% --------------------------------------------------------------------
\section{Interface}
% --------------------------------------------------------------------

\begin{code}
simpleGui = BG.BuilderGui {
      BG.generateGui = generateGui 
    , BG.debugGui = debugGui }

generateGui :: ProgStateRef -> IO ()
generateGui pstRef = 
  do res <- runGeni pstRef simpleBuilder 
     resultsGui pstRef res
\end{code}

% --------------------------------------------------------------------
\section{Results}
\label{sec:results_gui}
% --------------------------------------------------------------------

\paragraph{resultsGui} displays generation result in a window.  The window
consists of various tabs for intermediary results in lexical
selection, derived trees, derivation trees and generation statistics.

\begin{code}
resultsGui :: ProgStateRef -> ([String], SimpleStatus) -> IO () 
resultsGui pstRef (sentences, st) = 
 do -- results window
    f <- frame [ text := "Results" 
               , fullRepaintOnResize := False 
               , layout := stretch $ label "Generating..."
               , clientSize := sz 300 300 
               ] 
    p    <- panel f []
    nb   <- notebook p []
    -- realisations tab
    resTab <- realisationsGui pstRef nb (theResults st)
    -- statistics tab
    statTab <- statsGui nb (showRealisations sentences)
    -- pack it all together 
    set f [ layout := container p $ column 0 [ tabs nb 
          -- we put the realisations tab last because of what
          -- seems to be buggy behaviour wrt to wxhaskell 
          -- or wxWidgets 2.4 and the splitter
                 [ tab "summary"       statTab
                 , tab "realisations"  resTab ] ]
          , clientSize := sz 700 600 ] 
    return ()
\end{code}

\paragraph{statsGui} displays the generation statistics and provides a
handy button for saving results to a text file.

\begin{code}
statsGui :: (Window a) -> String -> IO Layout 
statsGui f msg = 
  do p <- panel f []
     -- sw <- scrolledWindow p [scrollRate := sz 10 10 ]
     t  <- textCtrl p [ text := msg, enabled := False ]
     saveBt <- button p [ text := "Save to file" 
                        , on command := saveCmd ]
     return (fill $ container p $ column 1 $ 
              [ fill $ widget t,
                hfloatRight $ widget saveBt ] )
  where
    saveCmd = 
      do let filetypes = [("Any file",["*","*.*"])]
         fsel <- fileSaveDialog f False True "Save to" filetypes "" "" 
         case fsel of
           Nothing   -> return () 
           Just file -> writeFile file msg
\end{code}

\subsection{Derived Trees}

Browser for derived/derivation trees, except if there are no results, we show a
message box

\begin{code}
realisationsGui :: ProgStateRef -> (Window a) -> [TagElem] -> IO Layout
realisationsGui _   f [] = messageGui f "No results found"
realisationsGui pstRef f resultsRaw = 
  do let tip = "result"
         itNlabl = map (\t -> (noHighlight t, toSentence t)) resultsRaw
            where noHighlight x = Just $ x { thighlight = [] }
     --
     pst     <- readIORef pstRef
     (lay,_) <- tagViewerGui pst f tip "derived" itNlabl
     return lay
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
    f <- frame [ text := "Geni Debugger" 
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
    let tabLabels = map (\x -> "session " ++ show x) [1..] 
        createTab (cd,xs)  = debuggerTab nb config initStuff2 cd  
          where initStuff2 = initStuff { B.inCands = xs }
    debugTabs <- mapM createTab $ zip tabLabels combos
    let genTabs = zipWith tab tabLabels debugTabs
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
 do let initBuilder = B.init  simpleBuilder
        nextStep    = B.step  simpleBuilder
        manySteps   = B.stepAll simpleBuilder
        genstats    = B.stats simpleBuilder
        --
    let initRes = []
        initSt  = initBuilder input (config {usetrash=True})
        (items,labels) = showGenState initRes initSt 
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
                 r2 = theResults s2
             setGvDrawables2 gvRef (showGenState r2 s2)
             setGvSel gvRef 1
             updaterFn
             updateStatsTxt (genstats s2)
             set nextBt [ on command :~ (\_ -> showNext s2) ]
    let showLast = 
          do -- redo generation from scratch
             let s = snd $ runState manySteps initSt 
                 r = theResults s
             setGvDrawables2 gvRef (showGenState r s)
             updaterFn
             updateStatsTxt (genstats s)
    let showReset = 
          do let res = initRes 
                 st  = initSt 
             set nextBt   [ on command  := showNext st ]
             updateStatsTxt (B.initGstats)
             setGvDrawables2 gvRef (showGenState res st)
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
showGenState :: [TagElem] -> SimpleStatus -> ([TagElem],[String])
showGenState res st = 
  let agenda    = theAgenda st
      auxiliary = theAuxAgenda st
      trash     = theTrash st
      chart     = theChart  st
      --
      trees     =  (emptyTE:agenda) 
                 ++ (emptyTE:chart) 
                 ++ (emptyTE:auxiliary) 
                 ++ (emptyTE:trash) 
                 ++ (emptyTE:res) 
      labels     =  ("___AGENDA___"    : (labelFn agenda))
                 ++ ("___CHART___"     : (labelFn chart))
                 ++ ("___AUXILIARY___" : (labelFn auxiliary))
                 ++ ("___DISCARDED___" : (labelFn trash)) 
                 ++ ("___RESULTS___"   : (labelFn res)) 
      --
      showPaths = if (chartsharing $ genconfig st)
                  then (\t -> " (" ++ showPolPaths t ++ ")")
                  else const ""
      labelFn trs = map (\t -> (toSentence t) ++ (showPaths t)) trs 
  in (trees,labels)
\end{code}
