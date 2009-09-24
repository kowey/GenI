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
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Simple.SimpleGui where
\end{code}

\ignore{
\begin{code}
import Graphics.UI.WX

import Control.Arrow ( (&&&) )
import Data.IORef
import qualified Data.Map as Map

import NLP.GenI.Statistics (Statistics)

import NLP.GenI.Btypes (GNode(gnname, gup), AvPair(..), emptyGNode, GeniVal(GConst))
import NLP.GenI.Configuration ( Params(..) )
import NLP.GenI.Geni ( ProgStateRef, runGeni, GeniResult )
import NLP.GenI.Graphviz ( GraphvizShow(..), gvNewline, gvUnlines )
import NLP.GenI.GuiHelper
  ( messageGui, tagViewerGui,
    debuggerPanel, DebuggerItemBar, setGvParams, GvIO, newGvRef, GraphvizGuiSt(..),
    viewTagWidgets, XMGDerivation(getSourceTrees),
  )
import NLP.GenI.Tags (tsemantics, DerivationStep(dsChild), TagElem(idname, ttree), TagItem(..), emptyTE)
import NLP.GenI.GraphvizShow ( graphvizShowDerivation )

import qualified NLP.GenI.Builder    as B
import NLP.GenI.Builder (LemmaPlus(..))
import qualified NLP.GenI.BuilderGui as BG
import NLP.GenI.Polarity
import NLP.GenI.Simple.SimpleBuilder
  ( simpleBuilder, SimpleStatus, SimpleItem(..), SimpleGuiItem(..)
  , unpackResult
  , step, theResults, theAgenda, theHoldingPen, theChart, theTrash)
\end{code}
}

% --------------------------------------------------------------------
\section{Interface}
% --------------------------------------------------------------------

\begin{code}
simpleGui_2p, simpleGui_1p :: BG.BuilderGui
simpleGui_2p = simpleGui True
simpleGui_1p = simpleGui False

simpleGui :: Bool -> BG.BuilderGui
simpleGui twophase = BG.BuilderGui {
      BG.resultsPnl  = resultsPnl twophase
    , BG.debuggerPnl = simpleDebuggerTab twophase }

resultsPnl :: Bool -> ProgStateRef -> Window a -> IO ([GeniResult], Statistics, Layout)
resultsPnl twophase pstRef f =
  do (sentences, stats, st) <- runGeni pstRef (simpleBuilder twophase)
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
                -> GvIO () Bool (Maybe SimpleItem)
realisationsGui _   f [] =
  do m <- messageGui f "No results found"
     g <- newGvRef () False ""
     return (m, g, return ())
realisationsGui pstRef f resultsRaw =
  do let tip = "result"
         itNlabl = map (Just &&& siToSentence) resultsRaw
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
simpleDebuggerTab twophase x1 (pa@x2) =
  debuggerPanel (simpleBuilder twophase) False stToGraphviz (simpleItemBar pa)
   x1 x2
 
stToGraphviz :: SimpleStatus -> [(Maybe SimpleItem, String)]
stToGraphviz st = 
  let agenda    = section "AGENDA"    $ theAgenda    st
      auxAgenda = section "HOLDING"   $ theHoldingPen st
      trash     = section "TRASH"     $ theTrash     st
      chart     = section "CHART"     $ theChart     st
      results   = section "RESULTS"   $ theResults   st
      --
      section n i = hd : (map tlFn i)
        where hd = (Nothing, "___" ++ n ++ "___")
              tlFn x = (Just x, siToSentence x ++ (showPaths $ siPolpaths x))
      showPaths t = " (" ++ showPolPaths t ++ ")"
  in concat [ agenda, auxAgenda, chart, trash, results ]

simpleItemBar :: Params -> DebuggerItemBar SimpleStatus Bool SimpleItem
simpleItemBar pa f gvRef updaterFn =
 do ib <- panel f []
    phaseTxt   <- staticText ib [ text := "" ]
    detailsChk <- checkBox ib [ text := "Show features"
                              , checked := False ]
    viewTagLay <- viewTagWidgets ib gvRef pa
    -- handlers
    let onDetailsChk = 
         do isDetailed <- get detailsChk checked 
            setGvParams gvRef isDetailed
            updaterFn
    set detailsChk [ on command := onDetailsChk ]
    --
    let lay = hfloatCentre . container ib . row 5 $
               [ hspace 5
               , widget phaseTxt
               , hglue
               , widget detailsChk
               , hglue
               , viewTagLay
               , hspace 5 ]
    let onUpdate =
          do status <- gvcore `fmap` readIORef gvRef
             set phaseTxt [ text := show (step status) ]
    return (lay, onUpdate)
\end{code}

% --------------------------------------------------------------------
\section{Miscellaneous}
% -------------------------------------------------------------------

\begin{code}
instance TagItem SimpleItem where
 tgIdName    = siIdname.siGuiStuff
 tgIdNum     = siId
 tgSemantics = siFullSem.siGuiStuff

instance XMGDerivation SimpleItem where
 -- Note: this is XMG-related stuff
 getSourceTrees it = tgIdName it : (map dsChild . siDerivation $ it)
\end{code}

\begin{code}
instance GraphvizShow Bool SimpleItem where
  graphvizLabel  f c =
    graphvizLabel f (toTagElem c) ++ gvNewline ++ (gvUnlines $ siDiagnostic $ siGuiStuff c)

  graphvizParams f c = graphvizParams f (toTagElem c)
  graphvizShowAsSubgraph f p it =
   let isHiglight n = gnname n `elem` (siHighlight.siGuiStuff) it
       info n | isHiglight n = (n, Just "red")
              | otherwise    = (n, Nothing)
   in    "\n// ------------------- elementary tree --------------------------\n"
      ++ graphvizShowAsSubgraph (f, info) (p ++ "TagElem") (toTagElem it)
      ++ "\n// ------------------- derivation tree --------------------------\n"
      -- derivation tree is displayed without any decoration
      ++ (graphvizShowDerivation . siDerivation $ it)

toTagElem :: SimpleItem -> TagElem
toTagElem si =
  emptyTE { idname = tgIdName si
          , tsemantics = tgSemantics si
          , ttree = fmap lookupOrBug (siDerived si) }
  where
   nodes   = siNodes.siGuiStuff $ si
   nodeMap = Map.fromList $ zip (map gnname nodes) nodes
   lookupOrBug k = case Map.lookup k nodeMap of
                   Nothing -> emptyGNode { gup = [ AvPair "cat" (GConst ["error looking up " ++ k]) ] }
                   Just x  -> x
\end{code}

\begin{code}
siToSentence :: SimpleItem -> String
siToSentence si = case unpackResult si of
                  []    -> siIdname.siGuiStuff $ si
                  (h:_) -> unwords . map lpLemma . fst $ h
\end{code}
