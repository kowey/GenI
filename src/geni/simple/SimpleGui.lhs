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

import Control.Monad (liftM)

import Data.IORef
import Data.List (find, nub, (\\))
import Data.Maybe (isJust)

import Geni 
  ( ProgState(..), ProgStateRef
  , initGeni, runGeni )
import Btypes 
  (showLexeme, GNode(gnname),
   iword, isemantics)
import Tags (tsemantics, TagElem(idname), TagItem(..))

import Configuration ( Params(..), polarised, chartsharing )
import General ( fst3, snd3 )
import Graphviz ( GraphvizShow(..), gvNewline, gvUnlines )
import GuiHelper
  ( toSentence,
    statsGui, messageGui, tagViewerGui, candidateGui, polarityGui,
    debuggerPanel, DebuggerItemBar, setGvParams, GvIO, newGvRef,
    XMGDerivation(getSourceTrees),
  )
import Tags ( ttreename )
import Treeprint ( graphvizShowDerivation )

import qualified Builder    as B
import qualified BuilderGui as BG 
import Polarity
import SimpleBuilder 
  ( simpleBuilder, SimpleStatus, SimpleItem(..), genconfig
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
    (resTab,_,_) <- realisationsGui pstRef nb (theResults st)
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
realisationsGui :: ProgStateRef -> (Window a) -> [SimpleItem]
                -> GvIO Bool (Maybe SimpleItem)
realisationsGui _   f [] =
  do m <- messageGui f "No results found"
     g <- newGvRef False [] ""
     return (m, g, return ())
realisationsGui pstRef f resultsRaw =
  do let tip = "result"
         itNlabl = map (\t -> (Just t, siToSentence t)) resultsRaw
     --
     pst     <- readIORef pstRef
     -- FIXME: have to show the semantics again
     tagViewerGui pst f tip "derived" itNlabl
\end{code}

% --------------------------------------------------------------------
\section{Debugger}
\label{sec:simple_debugger_gui}
\label{fn:simpleDebugGui}
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
        (cand,_)   = unzip $ B.inCands initStuff
        lexonly    = B.inLex initStuff 
    -- candidate selection tab
    let missedSem  = tsem \\ (nub $ concatMap tsemantics cand)
        -- we assume that for a tree to correspond to a lexical item,
        -- it must have the same semantics
        hasTree l = isJust $ find (\t -> tsemantics t == lsem) cand
          where lsem = isemantics l
        missedLex = [ showLexeme (iword l) | l <- lexonly, (not.hasTree) l ]
    (canPnl,_,_) <- candidateGui pst nb cand missedSem missedLex
    -- generation step 2.A (run polarity stuff)
    let (input2, _, autstuff) = B.preInit initStuff config
    -- automata tab
    let (auts, finalaut, _) = autstuff
    autPnl <- fst3 `liftM` polarityGui nb auts finalaut
    -- generation step 2.B (start the generator)
    debugPnl <- simpleDebuggerTab nb config input2 "simple" 
    let lexTab = tab "lexical selection" canPnl
        autTab = tab "automata" autPnl
        debugTab = tab "session" debugPnl
        genTabs = if polarised config then [ autTab, debugTab ] else [ debugTab ]
    --
    set f [ layout := container p $ column 0 [ tabs nb (lexTab : genTabs) ]
          , clientSize := sz 700 600 ]
    return ()
\end{code}
  
The generation could conceivably be broken into multiple generation
tasks, so we create a separate tab for each task.

\begin{code}
simpleDebuggerTab :: (Window a) -> Params -> B.Input -> String -> IO Layout 
simpleDebuggerTab = debuggerPanel simpleBuilder False stToGraphviz simpleItemBar
 
stToGraphviz :: SimpleStatus -> ([Maybe SimpleItem], [String])
stToGraphviz st = 
  let agenda    = section "AGENDA"    $ theAgenda    st
      auxAgenda = section "AUXILIARY" $ theAuxAgenda st
      trash     = section "TRASH"     $ theTrash     st
      chart     = section "CHART"     $ theChart     st
      results   = section "RESULTS"   $ theResults   st
      --
      section n i = hd : (map tlFn i)
        where hd = (Nothing, "___" ++ n ++ "___")
              tlFn x = (Just x, (siToSentence x) ++ (showPaths $ siPolpaths x))
      showPaths t = if (chartsharing $ genconfig st)
                    then " (" ++ showPolPaths t ++ ")"
                    else ""
  in unzip $ agenda ++ auxAgenda ++ chart ++ trash ++ results 

simpleItemBar :: DebuggerItemBar Bool SimpleItem
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

% --------------------------------------------------------------------
\section{Miscellaneous}
% -------------------------------------------------------------------

\begin{code}
instance TagItem SimpleItem where
 tgIdName    = idname.siTagElem
 tgIdNum     = siId
 tgSemantics = tsemantics.siTagElem

instance XMGDerivation SimpleItem where
 -- Note: this is XMG-related stuff
 getSourceTrees item =
  let -- strips all gorn addressing stuff
      stripGorn n = if dot `elem` n then stripGorn stripped else n
        where stripped = (tail $ dropWhile (/= dot) n)
              dot = '.'
      deriv  = map (stripGorn.snd3) $ snd $ siDerivation item
  in  nub ((ttreename.siTagElem) item : deriv)
\end{code}

\begin{code}
instance GraphvizShow Bool SimpleItem where
  graphvizLabel  f c =
    graphvizLabel f (siTagElem c) ++ gvNewline ++ (gvUnlines $ siDiagnostic c)

  graphvizParams f c = graphvizParams f (siTagElem c)
  graphvizShowAsSubgraph f p item =
   let isHiglight n = gnname n `elem` siHighlight item
       info n | isHiglight n = (n, Just "red")
              | otherwise    = (n, Nothing)
   in    "\n// ------------------- elementary tree --------------------------\n"
      ++ graphvizShowAsSubgraph (f, info) (p ++ "TagElem") (siTagElem item)
      ++ "\n// ------------------- derivation tree --------------------------\n"
      -- derivation tree is displayed without any decoration
      ++ (graphvizShowDerivation $ snd $ siDerivation item)
\end{code}

\begin{code}
siToSentence :: SimpleItem -> String
siToSentence = toSentence.siTagElem
\end{code}
