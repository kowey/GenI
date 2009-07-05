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

\chapter{GUI Helper} 

This module provides helper functions for building the GenI graphical
user interface

\begin{code}
{-# LANGUAGE FlexibleContexts #-}
module NLP.GenI.GuiHelper where
\end{code}

\ignore{
\begin{code}
import Graphics.UI.WX
-- import Graphics.UI.WXCore

import qualified Control.Monad as Monad 
import Control.Monad.State ( execStateT, runState )
import qualified Data.Map as Map

import Data.IORef
import Data.List (intersperse)
import System.Directory 
import System.FilePath ((<.>),(</>),dropExtensions)
import System.Process (runProcess)
import Text.ParserCombinators.Parsec (parseFromFile)

import NLP.GenI.Graphviz
import NLP.GenI.Automaton (numStates, numTransitions)
import NLP.GenI.Statistics (Statistics, showFinalStats)

import NLP.GenI.Configuration ( getFlagP, MacrosFlg(..), ViewCmdFlg(..) )
import NLP.GenI.GeniShow(geniShow)
import NLP.GenI.GraphvizShow ()
import NLP.GenI.Tags (TagItem(tgIdName), tagLeaves)
import NLP.GenI.Geni
  ( ProgState(..), showRealisations )
import NLP.GenI.GeniParsers ( geniTagElems )
import NLP.GenI.General
  (geniBug, boundsCheck, dropTillIncluding, ePutStrLn)
import NLP.GenI.Btypes
  ( showAv, showPred, showSem, showLexeme, Sem, ILexEntry(iword, ifamname), )
import NLP.GenI.PolarityTypes ( PolarityKey(..) )
import NLP.GenI.Tags
  ( idname, mapBySem, TagElem(ttrace, tinterface) )

import NLP.GenI.Configuration
  ( Params(..), MetricsFlg(..), setFlagP )

import qualified NLP.GenI.Builder as B
import NLP.GenI.Builder (queryCounter, num_iterations, chart_size,
    num_comparisons)
import NLP.GenI.Polarity (PolAut, detectPolFeatures)
import NLP.GenI.GraphvizShowPolarity ()
\end{code}
}

\subsection{Lexically selected items}

We have a browser for the lexically selected items.  We group the lexically
selected items by the semantics they subsume, inserting along the way some
fake trees and labels for the semantics.

The arguments \fnparam{missedSem} and \fnparam{missedLex} are used to 
indicate to the user respectively if any bits of the input semantics
have not been accounted for, or if there have been lexically selected
items for which no tree has been found.

\begin{code}
candidateGui :: ProgState -> (Window a) -> [TagElem] -> Sem -> [ILexEntry]
             -> GvIO Bool (Maybe TagElem)
candidateGui pst f xs missedSem missedLex = do
  p  <- panel f []      
  (tb,gvRef,updater) <- tagViewerGui pst p "lexically selected item" "candidates"
                        $ sectionsBySem xs
  let warningSem = if null missedSem then ""
                   else "WARNING: no lexical selection for " ++ showSem missedSem
      warningLex = if null missedLex then ""
                   else "WARNING: '" ++ (concat $ intersperse ", " $ map showLex missedLex)
                        ++ "' were lexically selected, but are not anchored to"
                        ++ " any trees"
                   where showLex l = (showLexeme $ iword l) ++ "-" ++ (ifamname l)
      --
      polFeats = "Polarity attributes detected: " ++ (unwords.detectPolFeatures) xs
      warning = unlines $ filter (not.null) [ warningSem, warningLex, polFeats ]
  -- side panel
  sidePnl <- panel p []
  ifaceLst <- singleListBox sidePnl [ tooltip := "interface for this tree (double-click me!)" ]
  traceLst <- singleListBox sidePnl [ tooltip := "trace for this tree (double-click me!)" ]
  tNoted <- textCtrl sidePnl [ wrap := WrapWord, text := "Hint: copy from below and paste into the sem:\n" ]
  let laySide = container sidePnl $ column 2
                  [ label "interface"
                  ,  fill $ widget ifaceLst
                  , label "trace"
                  ,  fill $ widget traceLst
                  , label "notes"
                  ,  fill $ widget tNoted ]
  -- handlers
  let addLine :: String -> String -> String
      addLine x y = y ++ "\n" ++ x
      --
      addToNoted w =
        do sel    <- get w selection
           things <- get w items
           when (sel > 0) $ set tNoted [ text :~ addLine (things !! sel) ]
  set ifaceLst [ on doubleClick := \_ -> addToNoted ifaceLst ]
  set traceLst [ on doubleClick := \_ -> addToNoted traceLst ]
  -- updaters : what happens when the user selects an item
  let updateTrace = gvOnSelect (return ())
        (\s -> set traceLst [ items := ttrace s ])
      updateIface = gvOnSelect (return ())
        (\s -> set ifaceLst [ items := map showAv $ tinterface s ])
  Monad.unless (null xs) $ do
    addGvHandler gvRef updateTrace
    addGvHandler gvRef updateIface
    -- first time run
    gvSt <- readIORef gvRef
    updateIface gvSt
    updateTrace gvSt
  --
  let layMain = fill $ row 2 [ fill tb, vfill laySide ]
      theItems = if null warning then [ layMain ] else [ hfill (label warning) , layMain ]
      lay  = fill $ container p $ column 5 theItems
  return (lay, gvRef, updater)

sectionsBySem :: (TagItem t) => [t] -> [ (Maybe t, String) ]
sectionsBySem tsem =
 let semmap   = mapBySem tsem
     sem      = Map.keys semmap
     --
     lookupTr k = Map.findWithDefault [] k semmap
     section  k = (Nothing, header) : (map tlab $ lookupTr k)
                  where header = "___" ++ showPred k ++ "___"
                        tlab t = (Just t, tgIdName t)
 in concatMap section sem
\end{code}
      
\subsection{Polarity Automata}

A browser to see the automata constructed during the polarity optimisation
step.

\begin{code}
polarityGui :: (Window a) -> [(PolarityKey,PolAut,PolAut)] -> PolAut
            -> GvIO () PolAut
polarityGui   f xs final = do
  let stats a = " (" ++ (show $ numStates a) ++ "st " ++ (show $ numTransitions a) ++ "tr)"
      aut2  (_ , a1, a2)  = [ a1, a2 ]
      autLabel (PolarityKey fv,a1,a2) = [ fv ++ stats a1, fv ++ " pruned" ++ stats a2]
      autlist = (concatMap aut2 xs) ++ [ final ]
      labels  = (concatMap autLabel xs) ++ [ "final" ++ stats final ]
      --
  gvRef   <- newGvRef () labels "automata"
  setGvDrawables gvRef autlist
  graphvizGui f "polarity" gvRef
\end{code}
      
\paragraph{statsGui} displays the generation statistics and provides a
handy button for saving results to a text file.

\begin{code}
statsGui :: (Window a) -> [String] -> Statistics -> IO Layout
statsGui f sentences stats =
  do let msg = showRealisations sentences
     --
     p <- panel f []
     t  <- textCtrl p [ text := msg, enabled := False ]
     statsTxt <- staticText p [ text := showFinalStats stats ]
     --
     saveBt <- button p [ text := "Save to file"
                        , on command := maybeSaveAsFile f msg ]
     return $ fill $ container p $ column 1 $
              [ hfill $ label "Performance data"
              , hfill $ widget statsTxt
              , hfill $ label "Realisations"
              , fill  $ widget t
              , hfloatRight $ widget saveBt ]
\end{code}

\subsection{TAG trees}

Our graphical interfaces have to display a great variety of items.  To
keep things nicely factorised, we define some type classes to describe
the things that these items may have in common.

\begin{code}
-- | Any data structure which has corresponds to a TAG tree and which
--   has some notion of derivation
class XMGDerivation a where
  getSourceTrees :: a -> [String]

instance XMGDerivation TagElem where
  getSourceTrees te = [idname te]
\end{code}

\fnlabel{toSentence} almost displays a TagElem as a sentence, but only
good enough for debugging needs.  The problem is that each leaf may be
an atomic disjunction. Our solution is just to display each choice and
use some delimiter to seperate them.  We also do not do any
morphological processing.

\begin{code}
toSentence :: TagElem -> String
toSentence = unwords . map squishLeaf . tagLeaves

squishLeaf :: (a,([String], b)) -> String
squishLeaf = showLexeme.fst.snd
\end{code}

\subsection{TAG viewer}

A TAG viewer is a graphvizGui that lets the user toggle the display
of TAG feature structures.

\begin{code}
tagViewerGui :: (GraphvizShow Bool t, TagItem t, XMGDerivation t)
             => ProgState -> (Window a) -> String -> String -> [(Maybe t,String)]
             -> GvIO Bool (Maybe t)
tagViewerGui pst f tip cachedir itNlab = do
  p <- panel f []      
  let (tagelems,labels) = unzip itNlab
  gvRef <- newGvRef False labels tip
  setGvDrawables gvRef tagelems 
  (lay,ref,updaterFn) <- graphvizGui p cachedir gvRef
  -- button bar widgets
  detailsChk <- checkBox p [ text := "Show features"
                           , checked := False ]
  viewTagLay <- viewTagWidgets p gvRef (pa pst)
  -- handlers
  let onDetailsChk =
        do isDetailed <- get detailsChk checked
           setGvParams gvRef isDetailed
           updaterFn
  set detailsChk [ on command := onDetailsChk ]
  -- pack it all in      
  let cmdBar = hfill $ row 5 
                [ dynamic $ widget detailsChk
                , viewTagLay ]
      lay2   = fill $ container p $ column 5 [ fill lay, cmdBar ]
  return (lay2,ref,updaterFn)
\end{code}

\subsection{XMG Metagrammar stuff}

XMG trees are produced by the XMG metagrammar system
(\url{http://sourcesup.cru.fr/xmg/}). To debug these grammars, it is useful,
given a TAG tree, to see what its metagrammar origins are.  We provide here an
interface to Yannick Parmentier's handy visualisation tool ViewTAG.

\begin{code}
viewTagWidgets :: (GraphvizShow Bool t, TagItem t, XMGDerivation t)
               => Window a -> GraphvizRef (Maybe t) Bool -> Params
               -> IO Layout
viewTagWidgets p gvRef config =
 do viewTagBtn <- button p [ text := "ViewTAG" ]
    viewTagCom <- choice p [ tooltip := "derivation tree" ]
    -- handlers
    let onViewTag = readIORef gvRef >>=
         gvOnSelect (return ())
           (\t -> do let derv = getSourceTrees t
                     ds <- get viewTagCom selection
                     if boundsCheck ds derv
                        then runViewTag config (derv !! ds)
                        else geniBug $ "Gui: bounds check in onViewTag"
           )
    set viewTagBtn [ on command := onViewTag ]
    -- when the user selects a tree, we want to update the list of derivations
    let updateDerivationList = gvOnSelect
          (set viewTagCom [ enabled := False ])
          (\s -> set viewTagCom [ enabled := True
                                , items := getSourceTrees s
                                , selection := 0] )
    addGvHandler gvRef updateDerivationList
    updateDerivationList =<< readIORef gvRef
    --
    return $ row 5 $ map dynamic [ widget viewTagCom, widget viewTagBtn ]

runViewTag :: Params -> String -> IO ()
runViewTag params drName =
  case getFlagP MacrosFlg params of
  Nothing -> ePutStrLn "Warning: No macros files specified (runViewTag)"
  Just f  -> do
     -- figure out what grammar file to use
     let gramfile = dropExtensions f <.> "rec"
         treenameOnly = takeWhile (/= ':') . dropTillIncluding ':' . dropTillIncluding ':'
     -- run the viewer
     case getFlagP ViewCmdFlg params of
       Nothing -> ePutStrLn "Warning: No viewcmd specified (runViewTag)"
       Just c  -> do -- run the viewer
                     runProcess c [gramfile, treenameOnly drName]
                       Nothing Nothing Nothing Nothing Nothing
                     return ()
\end{code}

% --------------------------------------------------------------------
\section{Graphical debugger}
\label{sec:debugger_helpers}
% --------------------------------------------------------------------

All GenI builders can make use of an interactive graphical debugger.  In
this section, we provide some helper code to build such a debugger.  

\paragraph{pauseOnLexGui} sometimes it is useful for the user to see the
lexical selection only and either dump it to file or read replace it by
the contents of some other file.  We provide an optional wrapper around
\fnref{candidateGui} which adds this extra functionality.  The wrapper
also includes a "Begin" button which runs your continuation on the new
lexical selection.

\begin{code}
pauseOnLexGui :: ProgState -> (Window a) -> [TagElem] -> Sem -> [ILexEntry]
              -> ([TagElem] -> IO ()) -- ^ continuation
              -> GvIO Bool (Maybe TagElem)
pauseOnLexGui pst f xs missedSem missedLex job = do
  p <- panel f []
  candV <- varCreate xs
  (tb, ref, updater) <- candidateGui pst p xs missedSem missedLex
  -- supplementary button bar
  let saveCmd =
       do c <- varGet candV
          let cStr = unlines $ map geniShow c
          maybeSaveAsFile f cStr
      loadCmd =
       do let filetypes = [("Any file",["*","*.*"])]
          fsel <- fileOpenDialog f False True "Choose your file..." filetypes "" ""
          case fsel of
           Nothing   -> return ()
           Just file ->
             do parsed <- parseFromFile geniTagElems file
                case parsed of
                 Left err -> errorDialog f "" (show err)
                 Right c  -> do varSet candV c
                                setGvDrawables2 ref (sectionsBySem c)
                                updater
  --
  saveBt <- button p [ text := "Save to file", on command := saveCmd ]
  loadBt <- button p [ text := "Load from file", on command := loadCmd ]
  nextBt <- button p [ text := "Begin" ]
  let disableW w = set w [ enabled := False ]
  set nextBt [ on command := do mapM disableW [ saveBt, loadBt, nextBt ]
                                varGet candV >>= job ]
  --
  let lay = fill $ container p $ column 5
            [ fill tb, hfill (vrule 1)
            , row 0 [ row 5 [ widget saveBt, widget loadBt ]
                    , hfloatRight $ widget nextBt ] ]
  return (lay, ref, updater)
\end{code}

\paragraph{debuggerTab} is potentially the most useful part of the
debugger.  It shows you the contents of chart, agenda and other
such structures used during the actual surface realisation process.
This may be a bit complicated to use because there is lots of extra
stuff you need to pass in order to parameterise the whole deal.

The function \fnreflite{debuggerTab} fills the parent window with the
standard components of a graphical debugger:
\begin{itemize}
\item An item viewer which allows the user to select one of the items
      in the builder state.
\item An item bar which provides some options on how to view the 
      currently selected item, for example, if you want to display the
      features or not.  
\item A dashboard which lets the user do things like ``go ahead 6
      steps''.
\end{itemize}

See the API for more details.

\begin{code}
type DebuggerItemBar flg itm 
      =  (Panel ())            -- ^ parent panel
      -> GraphvizRef (Maybe itm) flg   
      -- ^ gv ref to use
      -> GvUpdater -- ^ updaterFn
      -> IO Layout

-- | A generic graphical debugger widget for GenI
-- 
--   Besides the Builder, there are two functions you need to pass in make this
--   work: 
--
--      1. a 'stateToGv' which converts the builder state into a list of items
--         and labels the way 'graphvizGui' likes it
--
--      2. an 'item bar' function which lets you control what bits you display
--         of a selected item (for example, if you want a detailed view or not)
--         the item bar should return a layout 
--
--   Note that we don't constrain the type of item returned by the builder to
--   be the same as the type handled by your gui: that's quite normal because
--   you might want to decorate the type with some other information
debuggerPanel :: (GraphvizShow flg itm) 
  => B.Builder st itm2 Params -- ^ builder to use
  -> flg -- ^ initial value for the flag argument in GraphvizShow
  -> (st -> [(Maybe itm, String)])
     -- ^ function to convert a Builder state into lists of items
     --   and their labels, the way graphvizGui likes it
  -> (DebuggerItemBar flg itm)
     -- ^ 'itemBar' function returning a control panel configuring
     --   how you want the currently selected item in the debugger
     --   to be displayed
  -> (Window a) -- ^ parent window
  -> Params     -- ^ geni params
  -> B.Input    -- ^ builder input
  -> String     -- ^ graphviz cache directory
  -> IO Layout 
debuggerPanel builder gvInitial stateToGv itemBar f config input cachedir = 
 do let initBuilder = B.init  builder 
        nextStep    = B.step  builder 
        allSteps    = B.stepAll builder 
        --
    let (initS, initStats) = initBuilder input config2
        config2 = setFlagP MetricsFlg (B.defaultMetricNames) config
        (theItems,labels) = unzip $ stateToGv initS
    p <- panel f []      
    -- ---------------------------------------------------------
    -- item viewer: select and display an item
    -- ---------------------------------------------------------
    gvRef <- newGvRef gvInitial labels "debugger session" 
    setGvDrawables gvRef theItems
    (layItemViewer,_,updaterFn) <- graphvizGui p cachedir gvRef
    -- ----------------------------------------------------------
    -- item bar: controls for how an individual item is displayed
    -- ----------------------------------------------------------
    layItemBar <- itemBar p gvRef updaterFn
    -- ------------------------------------------- 
    -- dashboard: controls for the debugger itself 
    -- ------------------------------------------- 
    db <- panel p []
    restartBt <- button db [text := "Start over"]
    nextBt    <- button db [text := "Step by..."]
    leapVal   <- entry  db [ text := "1", clientSize := sz 30 25 ]
    finishBt  <- button db [text := "Leap to end"]
    statsTxt  <- staticText db []
    -- dashboard commands
    let showQuery c gs = case queryCounter c gs of
                         Nothing -> "???"
                         Just q  -> show q
        updateStatsTxt gs = set statsTxt [ text :~ (\_ -> txtStats gs) ]
        txtStats   gs =  "itr " ++ (showQuery num_iterations gs) ++ " "
                      ++ "chart sz: " ++ (showQuery chart_size gs)
                      ++ "\ncomparisons: " ++ (showQuery num_comparisons gs)
    let genStep _ (st,stats) = runState (execStateT nextStep st) stats
    let showNext s_stats = 
          do leapTxt <- get leapVal text
             let leapInt :: Integer
                 leapInt = read leapTxt
                 (s2,stats2) = foldr genStep s_stats [1..leapInt]
             setGvDrawables2 gvRef (stateToGv s2)
             setGvSel gvRef 1
             updaterFn
             updateStatsTxt stats2
             set nextBt [ on command :~ (\_ -> showNext (s2,stats2) ) ]
    let showLast = 
          do -- redo generation from scratch
             let (s2, stats2) = runState (execStateT allSteps initS) initStats 
             setGvDrawables2 gvRef (stateToGv s2)
             updaterFn
             updateStatsTxt stats2
    let showReset = 
          do set nextBt   [ on command  := showNext (initS, initStats) ]
             updateStatsTxt initStats 
             setGvDrawables2 gvRef (stateToGv initS)
             setGvSel gvRef 1
             updaterFn
    -- dashboard handlers
    set finishBt  [ on command := showLast ]
    set restartBt [ on command := showReset ]
    showReset
    -- dashboard layout  
    let layCmdBar = hfill $ container db $ row 5
                     [ widget statsTxt, hfloatRight $ row 5 
                       [ widget restartBt, widget nextBt 
                       , widget leapVal, label " step(s)"
                       , widget finishBt ] ]
    -- ------------------------------------------- 
    -- overall layout
    -- ------------------------------------------- 
    return $ fill $ container p $ column 5 [ layItemViewer, layItemBar, hfill (vrule 1), layCmdBar ] 
\end{code}

% --------------------------------------------------------------------
\section{Graphviz GUI}
\label{sec:graphviz_gui}
% --------------------------------------------------------------------

A general-purpose GUI for displaying a list of items graphically via
AT\&T's excellent Graphviz utility.  We have a list box where we display
all the labels the user provided.  If the user selects an entry from
this box, then the item corresponding to that label will be displayed.
See section \ref{sec:draw_item}.

\paragraph{gvRef}

We use IORef as a way to keep track of the gui state and to provide you
the possibility for modifying the contents of the GUI.  The idea is that 

\begin{enumerate}
\item you create a GvRef with newGvRef
\item you call graphvizGui and get back an updater function
\item whenever you want to modify something, you use setGvWhatever
      and call the updater function
\item if you want to react to the selection being changed,
      you should set gvhandler
\end{enumerate}

\begin{code}
data GraphvizOrder = GvoParams | GvoItems | GvoSel 
     deriving Eq
data GraphvizGuiSt a b = 
        GvSt { gvitems   :: Map.Map Int a,
               gvparams  :: b,
               gvlabels  :: [String],
               -- tooltip for the selection box
               gvtip     :: String, 
               -- handler function to call when the selection is
               -- updated (note: before displaying the object)
               gvhandler :: Maybe (GraphvizGuiSt a b -> IO ()),
               gvsel     :: Int,
               gvorders  :: [GraphvizOrder] }
type GraphvizRef a b = IORef (GraphvizGuiSt a b)

newGvRef :: forall a . forall b . b -> [String] -> String -> IO (GraphvizRef a b)
newGvRef p l t =
  let st = GvSt { gvparams = p,
                  gvitems  = Map.empty,
                  gvlabels  = l, 
                  gvhandler = Nothing,
                  gvtip    = t,
                  gvsel    = 0,
                  gvorders = [] }
  in newIORef st

setGvSel :: GraphvizRef a b  -> Int -> IO ()
setGvSel gvref s  =
  do let fn x = x { gvsel = s,
                    gvorders = GvoSel : (gvorders x) }
     modifyIORef gvref fn 
  
setGvParams :: GraphvizRef a b -> b -> IO ()
setGvParams gvref c  =
  do let fn x = x { gvparams = c,
                    gvorders = GvoParams : (gvorders x) }
     modifyIORef gvref fn 

modifyGvParams :: GraphvizRef a b -> (b -> b) -> IO ()
modifyGvParams gvref fn  =
  do gvSt <- readIORef gvref
     setGvParams gvref (fn $ gvparams gvSt)

setGvDrawables :: GraphvizRef a b -> [a] -> IO ()
setGvDrawables gvref it =
  do let fn x = x { gvitems = Map.fromList $ zip [0..] it,
                    gvorders = GvoItems : (gvorders x) }
     modifyIORef gvref fn 

setGvDrawables2 :: GraphvizRef a b -> [(a,String)] -> IO ()
setGvDrawables2 gvref itlb =
  do let (it,lb) = unzip itlb
         fn x = x { gvlabels = lb }
     modifyIORef gvref fn 
     setGvDrawables gvref it

-- | Helper function for making selection handlers (see 'addGvHandler')
--   Note that this was designed for cases where the contents is a Maybe
gvOnSelect :: IO () -> (a -> IO ()) -> GraphvizGuiSt (Maybe a) b -> IO ()
gvOnSelect onNothing onJust gvSt =
 let sel    = gvsel gvSt
     things = gvitems gvSt
 in case Map.lookup sel things of
    Just (Just s) -> onJust s
    _             -> onNothing

setGvHandler :: GraphvizRef a b -> Maybe (GraphvizGuiSt a b -> IO ()) -> IO ()
setGvHandler gvref mh =
  do gvSt <- readIORef gvref
     modifyIORef gvref (\x -> x { gvhandler = mh })
     case mh of 
       Nothing -> return ()
       Just fn -> fn gvSt

-- | add a selection handler - if there already is a handler
--   this handler will be called before the new one
addGvHandler :: GraphvizRef a b -> (GraphvizGuiSt a b -> IO ()) -> IO ()
addGvHandler gvref h =
  do gvSt <- readIORef gvref
     let newH = case gvhandler gvSt of 
                Nothing   -> Just h
                Just oldH -> Just (\g -> oldH g >> h g)
     setGvHandler gvref newH
\end{code}

\paragraph{graphvizGui} returns a layout (wxhaskell container) and a
function for updating the contents of this GUI.

Arguments:
\begin{enumerate}
\item f - (parent window) the GUI is provided as a panel within the parent.
          Note: we use window in the WxWidget's sense, meaning it could be
          anything as simple as a another panel, or a notebook tab.
\item glab - (gui labels) a tuple of strings (tooltip, next button text)
\item cachedir - the cache subdirectory.  We intialise this by creating a cache
          directory for images which will be generated from the results
\item gvRef - see above
\end{enumerate}

Returns: a function for updating the GUI.  FIXME: it's not entirely clear
what the updater function is for; note that it's not the same as the 
handler function!

\begin{code}
graphvizGui :: (GraphvizShow f d) => 
  (Window a) -> String -> GraphvizRef d f -> GvIO f d
type GvIO f d  = IO (Layout, GraphvizRef d f, GvUpdater)
type GvUpdater = IO ()

graphvizGui f cachedir gvRef = do
  initGvSt <- readIORef gvRef
  -- widgets
  p <- panel f [ fullRepaintOnResize := False ]
  split <- splitterWindow p []
  (dtBitmap,sw) <- scrolledBitmap split 
  rchoice  <- singleListBox split [tooltip := gvtip initGvSt]
  -- set handlers
  let openFn   = openImage sw dtBitmap 
  -- pack it all together
  let lay = fill $ container p $ margin 1 $ fill $ 
            vsplit split 5 200 (widget rchoice) (widget sw) 
  set p [ on closing := closeImage dtBitmap ]
  -- bind an action to rchoice
  let showItem = do createAndOpenImage cachedir p gvRef openFn
                 `catch` \e -> errorDialog f "" (show e)
  ------------------------------------------------
  -- create an updater function
  ------------------------------------------------
  let updaterFn = do 
        gvSt <- readIORef gvRef
        let orders = gvorders gvSt 
            labels = gvlabels gvSt
            sel    = gvsel    gvSt
        initCacheDir cachedir 
        Monad.when (GvoItems `elem` orders) $ 
          set rchoice [ items :~ (\_ -> labels) ]
        Monad.when (GvoSel `elem` orders) $
          set rchoice [ selection :~ (\_ -> sel) ]
        modifyIORef gvRef (\x -> x { gvorders = []})
        -- putStrLn "updaterFn called" 
        showItem 
  ------------------------------------------------
  -- enable the tree selector
  -- FIXME: not sure that this is correct
  ------------------------------------------------
  let selectAndShow = do
        -- putStrLn "selectAndShow called" 
        sel  <- get rchoice selection
        -- note: do not use setGvSel (infinite loop)
        modifyIORef gvRef (\x -> x { gvsel = sel })
        -- call the handler if there is one 
        gvSt <- readIORef gvRef
        case (gvhandler gvSt) of 
          Nothing -> return ()
          Just h  -> h gvSt
        -- now do the update
        updaterFn
  ------------------------------------------------
  set rchoice [ on select := selectAndShow ]
  -- call the updater function for the first time
  -- setGvSel gvRef 1
  updaterFn 
  -- return the layout, the gvRef, and an updater function
  -- The gvRef is to make it easier for users to muck around with the
  -- state of the gui.  Here, it's trivial, but when people combine guis
  -- together, it might be easier to keep track of when returned
  return (lay, gvRef, updaterFn)
\end{code}

\subsection{Scroll bitmap}

Bitmap with a scrollbar

\begin{code}
scrolledBitmap :: Window a -> IO(VarBitmap, ScrolledWindow ())
scrolledBitmap p = do
  dtBitmap <- variable [value := Nothing]
  sw       <- scrolledWindow p [scrollRate := sz 10 10, bgcolor := white,
                                on paint := onPaint dtBitmap,
                                fullRepaintOnResize := False ]       
  return (dtBitmap, sw)
\end{code}

\subsection{Bitmap functions}

The following helper functions were taken directly from the WxHaskell
sample code.

\begin{code}
type OpenImageFn = FilePath -> IO ()
type VarBitmap   = Var (Maybe (Bitmap ())) 

openImage :: Window a -> VarBitmap -> OpenImageFn
openImage sw vbitmap fname = do 
    -- load the new bitmap
    bm <- bitmapCreateFromFile fname  -- can fail with exception
    closeImage vbitmap
    set vbitmap [value := Just bm]
    -- reset the scrollbars 
    bmsize <- get bm size 
    set sw [virtualSize := bmsize]
    repaint sw
      `catch` \_ -> repaint sw

closeImage :: VarBitmap -> IO ()
closeImage vbitmap = do 
    mbBitmap <- swap vbitmap value Nothing
    case mbBitmap of
        Nothing -> return ()
        Just bm -> objectDelete bm

onPaint :: VarBitmap -> DC a -> b -> IO ()
onPaint vbitmap dc _ = do 
    mbBitmap <- get vbitmap value
    case mbBitmap of
      Nothing -> return () 
      Just bm -> do dcClear dc
                    drawBitmap dc bm pointZero False []
\end{code}

\subsection{Drawing stuff}
\label{sec:draw_item}

\paragraph{createAndOpenImage} Attempts to draw an image 
(or retrieve it from cache) and opens it if we succeed.  Otherwise, it
does nothing at all; the creation function will display an error message
if it fails.

\begin{code}
createAndOpenImage :: (GraphvizShow f b) => 
  FilePath -> Window a -> GraphvizRef b f -> OpenImageFn -> IO ()
createAndOpenImage cachedir f gvref openFn = do 
  let errormsg g = "The file " ++ g ++ " was not created!\n"
                   ++ "Is graphviz installed?"
  r <- createImage cachedir f gvref 
  case r of 
    Just graphic -> do exists <- doesFileExist graphic 
                       if exists 
                          then openFn graphic
                          else fail (errormsg graphic)
    Nothing      -> return ()

-- | Creates a graphical visualisation for anything which can be displayed
--   by graphviz.
createImage :: (GraphvizShow f b)
            => FilePath          -- ^ cache directory
            -> Window a          -- ^ parent window
            -> GraphvizRef b f   -- ^ stuff to display
            -> IO (Maybe FilePath)
createImage cachedir f gvref = do
  gvSt <- readIORef gvref
  -- putStrLn $ "creating image via graphviz"
  let drawables = gvitems  gvSt
      sel       = gvsel    gvSt
      config    = gvparams gvSt
  dotFile <- createDotPath cachedir (show sel)
  graphicFile <-  createImagePath cachedir (show sel)
  let create x = do toGraphviz config x dotFile graphicFile
                    return (Just graphicFile)
      handler err = do errorDialog f "Error calling graphviz" (show err) 
                       return Nothing
  exists <- doesFileExist graphicFile
  -- we only call graphviz if the image is not in the cache
  if exists
     then return (Just graphicFile)
     else case Map.lookup sel drawables of
            Nothing -> return Nothing
            Just it -> create it `catch` handler
\end{code}

\subsection{Cache directory}

We create a directory to put image files in so that we can avoid regenerating
images.  If the directory already exists, we can just delete all the files
in it.

\begin{code}
initCacheDir :: String -> IO()
initCacheDir cachesubdir = do 
  mainCacheDir <- gv_CACHEDIR
  cmainExists  <- doesDirectoryExist mainCacheDir 
  Monad.when (not cmainExists) $ createDirectory mainCacheDir 
  -- 
  let cachedir = mainCacheDir </> cachesubdir
  cExists    <- doesDirectoryExist cachedir
  if (cExists)
    then do let notdot x = (x /= "." && x /= "..")
            contents <- getDirectoryContents cachedir
            olddir <- getCurrentDirectory
            setCurrentDirectory cachedir
            mapM removeFile $ filter notdot contents
            setCurrentDirectory olddir
            return ()
    else createDirectory cachedir
\end{code}

\section{Miscellaneous}
\label{sec:gui_misc}

\begin{code}
-- | Save the given string to a file, if the user selets one via the file save
--   dialog. Otherwise, don't do anything.
maybeSaveAsFile :: (Window a) -> String -> IO ()
maybeSaveAsFile f msg =
 do let filetypes = [("Any file",["*","*.*"])]
    fsel <- fileSaveDialog f False True "Save to" filetypes "" ""
    case fsel of
      Nothing   -> return ()
      Just file -> writeFile file msg

-- | A message panel for use by the Results gui panels.
messageGui :: (Window a) -> String -> IO Layout 
messageGui f msg = do 
  p <- panel f []
  -- sw <- scrolledWindow p [scrollRate := sz 10 10 ]
  t  <- textCtrl p [ text := msg, enabled := False ]
  return (fill $ container p $ column 1 $ [ fill $ widget t ]) 
\end{code}

\begin{code}
gv_CACHEDIR :: IO String
gv_CACHEDIR = do
  home <- getHomeDirectory
  return $ home </> ".gvcache"

createImagePath :: String -> String -> IO String
createImagePath subdir name = do
  cdir <- gv_CACHEDIR
  return $ cdir </> subdir </> name <.> "png"

createDotPath :: String -> String -> IO String
createDotPath subdir name = do 
  cdir <- gv_CACHEDIR
  return $ cdir </> subdir </> name <.> "dot"
\end{code}


