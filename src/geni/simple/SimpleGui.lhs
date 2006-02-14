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

\chapter{Simple GUI}

\begin{code}
module SimpleGui where
\end{code}

\ignore{
\begin{code}
import Graphics.UI.WX
import Graphics.UI.WXCore

import qualified Control.Monad as Monad 

import Data.IORef
import Data.List (find, nub, (\\))
import Data.Maybe (isJust)

import Geni 
  ( ProgState(..), ProgStateRef
  , initGeni, runGeni )
import Btypes 
  (showLexeme,
   iword, isemantics)
import Tags (tsemantics,thighlight, TagElem)

import Configuration ( Params(..), polarised, chartsharing )
import GuiHelper

import qualified Builder    as B
import qualified BuilderGui as BG 
import Polarity
import SimpleBuilder 
  ( simpleBuilder, setup, SimpleStatus, genconfig 
  , theResults, theAgenda, theAuxAgenda, theChart, theTrash)
import Statistics (Statistics)
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
resultsGui :: ProgStateRef -> ([String], Statistics, SimpleStatus) -> IO ()
resultsGui pstRef (sentences, stats, st) =
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
    statTab <- statsGui nb sentences stats
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
        createTab (cd,xs)  = simpleDebuggerTab nb config initStuff2 cd  
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
simpleDebuggerTab :: (Window a) -> Params -> B.Input -> String -> IO Layout 
simpleDebuggerTab = debuggerPanel simpleBuilder False stToGraphviz simpleItemBar
 
stToGraphviz :: SimpleStatus -> ([Maybe TagElem], [String])
stToGraphviz st = 
  let agenda    = section "AGENDA"    $ theAgenda    st
      auxAgenda = section "AUXILIARY" $ theAuxAgenda st
      trash     = section "TRASH"     $ theTrash     st
      chart     = section "CHART"     $ theChart     st
      results   = section "RESULTS"   $ theResults   st
      --
      section n i = hd : (map tlFn i)
        where hd = (Nothing, "___" ++ n ++ "___")
              tlFn x = (Just x, toSentence x ++ (showPaths x))
      showPaths t = if (chartsharing $ genconfig st)
                    then " (" ++ showPolPaths t ++ ")"
                    else ""
  in unzip $ agenda ++ auxAgenda ++ chart ++ trash ++ results 

simpleItemBar :: DebuggerItemBar Bool TagElem 
simpleItemBar f gvRef updaterFn =
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
