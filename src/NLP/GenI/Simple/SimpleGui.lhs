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
module NLP.GenI.Simple.SimpleGui where
\end{code}

\ignore{
\begin{code}
import Graphics.UI.WX
import Graphics.UI.WXCore

import Data.IORef
import Data.List (nub)



import NLP.GenI.Btypes (GNode(gnname))
import NLP.GenI.Configuration ( Params(..) )
import NLP.GenI.General ( snd3 )
import NLP.GenI.Geni ( ProgStateRef, runGeni )
import NLP.GenI.Graphviz ( GraphvizShow(..), gvNewline, gvUnlines )
import NLP.GenI.GuiHelper
  ( toSentence,
    messageGui, tagViewerGui,
    debuggerPanel, DebuggerItemBar, setGvParams, GvIO, newGvRef,
    XMGDerivation(getSourceTrees),
  )
import NLP.GenI.Tags (tsemantics, TagElem(idname), TagItem(..), ttreename)
import NLP.GenI.Treeprint ( graphvizShowDerivation )

import qualified NLP.GenI.Builder    as B
import qualified NLP.GenI.BuilderGui as BG
import NLP.GenI.Polarity
import NLP.GenI.Simple.SimpleBuilder
  ( simpleBuilder, SimpleStatus, SimpleItem(..),
  , theResults, theAgenda, theAuxAgenda, theChart, theTrash)
\end{code}
}

% --------------------------------------------------------------------
\section{Interface}
% --------------------------------------------------------------------

\begin{code}
simpleGui_2p = simpleGui True
simpleGui_1p = simpleGui False

simpleGui twophase = BG.BuilderGui {
      BG.resultsPnl  = resultsPnl twophase
    , BG.debuggerPnl = simpleDebuggerTab twophase }

resultsPnl twophase pstRef f =
  do (sentences, stats, st) <- runGeni Nothing pstRef (simpleBuilder twophase)
     (lay, _, _) <- realisationsGui pstRef f (theResults st)
     return (sentences, stats, lay)
\end{code}

% --------------------------------------------------------------------
\section{Results}
\label{sec:results_gui}
% --------------------------------------------------------------------

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

\begin{code}
simpleDebuggerTab :: Bool -> (Window a) -> Params -> B.Input -> String -> IO Layout
simpleDebuggerTab twophase =
  debuggerPanel (simpleBuilder twophase) False stToGraphviz simpleItemBar
 
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
      showPaths t = " (" ++ showPolPaths t ++ ")"
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
