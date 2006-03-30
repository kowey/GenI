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
module GuiHelper where
\end{code}

\ignore{
\begin{code}
import Graphics.UI.WX
import Graphics.UI.WXCore

import qualified Control.Monad as Monad 
import Control.Monad.State ( execStateT, runState )
import qualified Data.Map as Map

import Data.Array
import Data.IORef
import Data.List (intersperse)
import System.Directory 
import System.Process (runProcess)

import Graphviz 
import Treeprint() -- only import the GraphvizShow instances

import Tags (TagItem(tgIdName), tagLeaves)
import Geni 
  ( ProgState(..), showRealisations )
import General (boundsCheck, slash, geniBug, ePutStrLn)
import Btypes 
  ( showPred, showSem, showLexeme
  , Sem)
import Tags 
  ( idname, mapBySem, TagElem )

import Configuration(Params(..), GrammarType(..))

import Automaton (states)
import qualified Builder as B
import Builder (queryCounter, num_iterations, chart_size,
    num_comparisons)
import Polarity (PolAut, detectPolFeatures, detectSansIdx)
import Statistics (Statistics, showFinalStats)

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
candidateGui :: ProgState -> (Window a) -> [TagElem] -> Sem -> [String]
             -> GvIO Bool (Maybe TagElem)
candidateGui pst f xs missedSem missedLex = do
  p  <- panel f []      
  (tb,ref,updater) <- tagViewerGui pst p "lexically selected item" "candidates"
                      $ sectionsBySem xs
  let warningSem = if null missedSem then ""
                   else "WARNING: no lexical selection for " ++ showSem missedSem
      warningLex = if null missedLex then ""
                   else "WARNING: '" ++ (concat $ intersperse ", " missedLex)
                        ++ "' were lexically selected, but are not anchored to"
                        ++ " any trees"
      -- FIXME: get rid of this at earliest convenience (2006-03-30)
      treesSansIdx_ = map idname $ detectSansIdx xs
      treesSansIdx = "Trees without idx: " ++ (unlines $ map unwords $ everyN 6 treesSansIdx_)
      everyN :: Int -> [a] -> [[a]]
      everyN n = helper
        where helper xs | length xs < n = [xs]
              helper xs = a : helper b where (a,b) = (splitAt n) xs
      --
      polFeats = "Polarity attributes detected: " ++ (unwords.detectPolFeatures) xs
      warning = unlines $ filter (not.null) [ warningSem, warningLex, polFeats, treesSansIdx ]
      items = if null warning then [ fill tb ] else [ hfill (label warning) , fill tb ]
      lay   = fill $ container p $ column 5 items
  ePutStrLn $ unlines treesSansIdx_ 
  return (lay, ref, updater)

sectionsBySem :: (TagItem t) => [t] -> [ (Maybe t, String) ]
sectionsBySem ts =
 let semmap   = mapBySem ts
     sem      = Map.keys semmap
     --
     lookupTr k = Map.findWithDefault [] k semmap
     section  k = (Nothing, header) : (map (\t -> (Just t, tgIdName t)) $ lookupTr k)
                  where header = "___" ++ showPred k ++ "___"
 in concatMap section sem
\end{code}
      
\subsection{Polarity Automata}

A browser to see the automata constructed during the polarity optimisation
step.

\begin{code}
polarityGui :: (Window a) -> [(String,PolAut,PolAut)] -> PolAut
            -> GvIO () PolAut
polarityGui   f xs final = do
  let numsts a = " : " ++ (show n) ++ " states" 
                 where n = foldr (+) 0 $ map length $ states a 
      aut2  (_ , a1, a2) = [ a1, a2 ]
      autLabel (fv,a1,_) = [ fv ++ numsts a1, fv ++ " pruned" ]
      autlist = (concatMap aut2 xs) ++ [ final ] 
      labels  = (concatMap autLabel xs) ++ [ "final" ++ numsts final ]
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
toSentence = unwords . (map squishLeaf) . tagLeaves

squishLeaf :: ([String], a) -> String
squishLeaf = showLexeme.fst 
\end{code}

\subsection{TAG viewer}

A TAG viewer is a graphvizGui that lets the user toggle the display
of TAG feature structures.

\begin{code}
tagViewerGui :: (GraphvizShow Bool t, TagItem t, XMGDerivation t)
             => ProgState -> (Window a) -> String -> String -> [(Maybe t,String)]
             -> GvIO Bool (Maybe t)
tagViewerGui pst f tip cachedir itNlab = do
  let config = pa pst
  p <- panel f []      
  let (tagelems,labels) = unzip itNlab
  gvRef <- newGvRef False labels tip
  setGvDrawables gvRef tagelems 
  (lay,ref,updaterFn) <- graphvizGui p cachedir gvRef
  -- widgets
  detailsChk <- checkBox p [ text := "Show features"
                           , checked := False ]
  displayTraceBut <- button p [ text := "Display trace for" ]
  displayTraceCom <- choice p [ tooltip := "derivation tree" ]
  -- handlers
  let onDisplayTrace 
       = do gvSt <- readIORef gvRef
            s <- get displayTraceCom selection
            let tsel = gvsel gvSt
            Monad.when (boundsCheck tsel tagelems) $ do
            let tree = tagelems !! (gvsel gvSt)
            case tree of 
             Nothing -> set displayTraceCom [ enabled := False ]
             Just t  -> let derv = getSourceTrees t in
                        if boundsCheck s derv 
                           then runViewTag pst (derv !! s)
                           else geniBug $ "Gui: bounds check in onDisplayTrace" 
  let onDetailsChk c 
       = do isDetailed <- get c checked 
            setGvParams gvRef isDetailed 
            updaterFn 
  let selHandler gvSt = do
      let tsel = gvsel gvSt
      Monad.when (boundsCheck tsel tagelems) $ do
        let selected = tagelems !! tsel 
        case selected of 
         Nothing -> set displayTraceCom [ enabled := False ]
         Just s  -> set displayTraceCom 
                     [ enabled := True, items := getSourceTrees s, selection := 0 ]
  --
  Monad.when (not $ null tagelems) $ do 
    addGvHandler gvRef selHandler
    set detailsChk [ on command := onDetailsChk detailsChk ]
    set displayTraceBut 
         [ on command := onDisplayTrace 
         , enabled    := grammarType config == XMGTools ] 
  -- pack it all in      
  let cmdBar = hfill $ row 5 
                [ dynamic $ widget detailsChk
                , dynamic $ widget displayTraceBut
                , dynamic $ widget displayTraceCom 
                ]
      lay2   = fill $ container p $ column 5 [ lay, cmdBar ] 
  return (lay2,ref,updaterFn)
\end{code}

% --------------------------------------------------------------------
\section{Graphical debugger}
% --------------------------------------------------------------------

All GenI builders can make use of an interactive graphical debugger.  In
this section, we provide some helper code to build such a debugger.  The
function \fnreflite{debuggerTab} fills the parent window with the
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
debuggerPanel :: (GraphvizShow flg itm) 
  => B.Builder st itm Params -- ^ builder to use
  -> flg -- ^ initial value for the flag argument in GraphvizShow
  -> (st -> ([Maybe itm], [String])) 
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
        config2 = config { metricsParam = B.defaultMetricNames }
        (items,labels) = stateToGv initS 
    p <- panel f []      
    -- ---------------------------------------------------------
    -- item viewer: select and display an item
    -- ---------------------------------------------------------
    gvRef <- newGvRef gvInitial labels "debugger session" 
    setGvDrawables gvRef items 
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
    nextBt    <- button db [text := "Leap by..."]
    leapVal   <- entry  db [ text := "1", clientSize := sz 30 25 ]
    finishBt  <- button db [text := "Continue"]
    statsTxt  <- staticText db []
    -- dashboard commands
    let showQuery c gs = case queryCounter c gs of
                         Nothing -> "???"
                         Just c  -> show c
        updateStatsTxt gs = set statsTxt [ text :~ (\_ -> txtStats gs) ]
        txtStats   gs =  "itr " ++ (showQuery num_iterations gs) ++ " "
                      ++ "chart sz: " ++ (showQuery chart_size gs)
                      ++ "\ncomparisons: " ++ (showQuery num_comparisons gs)
    let genStep _ (st,stats) = runState (execStateT nextStep st) stats
    let showNext s_stats = 
          do leapTxt <- get leapVal text
             let leapInt = read leapTxt
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
        GvSt { gvitems   :: Array Int a,
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

newGvRef p l t =
  let st = GvSt { gvparams = p,
                  gvitems  = array (0,0) [],
                  gvlabels  = l, 
                  gvhandler = Nothing,
                  gvtip    = t,
                  gvsel    = 0,
                  gvorders = [] }
  in newIORef st

setGvSel gvref s  =
  do let fn x = x { gvsel = s,
                    gvorders = GvoSel : (gvorders x) }
     modifyIORef gvref fn 
  
setGvParams gvref c  =
  do let fn x = x { gvparams = c,
                    gvorders = GvoParams : (gvorders x) }
     modifyIORef gvref fn 

modifyGvParams gvref fn  =
  do gvSt <- readIORef gvref
     setGvParams gvref (fn $ gvparams gvSt)

setGvDrawables gvref it =
  do let fn x = x { gvitems = array (0, length it) (zip [0..] it),
                    gvorders = GvoItems : (gvorders x) }
     modifyIORef gvref fn 

setGvDrawables2 gvref (it,lb) =
  do let fn x = x { gvlabels = lb }
     modifyIORef gvref fn 
     setGvDrawables gvref it

setGvHandler gvref mh =
  do gvSt <- readIORef gvref
     modifyIORef gvref (\x -> x { gvhandler = mh })
     case mh of 
       Nothing -> return ()
       Just fn -> fn gvSt

-- | add a selection handler - if there already is a handler
--   this handler will be called before the new one
addGvHandler gvref h =
  do gvSt <- readIORef gvref
     let newH = case gvhandler gvSt of 
                Nothing   -> Just h
                Just oldH -> Just (oldH >> h)
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
\end{code}

\paragraph{createImage}
Creates a graphical visualisation for anything which can be displayed
by graphviz. Arguments: a cache directory, a WxHaskell window, and index and an
array of trees.  Returns Just filename if the index is valid or Nothing
otherwise 

\begin{code}
createImage :: (GraphvizShow f b) => 
  FilePath -> Window a -> GraphvizRef b f -> IO (Maybe FilePath) 
createImage cachedir f gvref = do
  gvSt <- readIORef gvref
  -- putStrLn $ "creating image via graphviz"
  let drawables = gvitems  gvSt
      sel       = gvsel    gvSt
      config    = gvparams gvSt
      te = (drawables ! sel)
      b  = bounds drawables 
  dotFile <- createDotPath cachedir (show sel)
  graphicFile <-  createImagePath cachedir (show sel)
  let create = do toGraphviz config te dotFile graphicFile
                  return (Just graphicFile)
      handler err = do errorDialog f "Error calling graphviz" (show err) 
                       return Nothing
  exists <- doesFileExist graphicFile
  -- we only call graphviz if the image is not in the cache
  if (exists) 
     then return (Just graphicFile)
     else if (sel >= fst b && sel < snd b)
             then create `catch` handler 
             else return Nothing
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
  let cachedir = mainCacheDir ++ slash ++ cachesubdir  
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
  return $ home ++ slash ++ ".gvcache"

createImagePath :: String -> String -> IO String
createImagePath subdir name = do
  cdir <- gv_CACHEDIR
  return $ cdir ++ slash ++ subdir ++ slash ++ name ++ ".png"

createDotPath :: String -> String -> IO String
createDotPath subdir name = do 
  cdir <- gv_CACHEDIR
  return $ cdir ++ slash ++ subdir ++ slash ++ name ++ ".dot"
\end{code}

\subsection{XMG Metagrammar stuff}

XMG trees are produced by the XMG metagrammar system
(\url{http://sourcesup.cru.fr/xmg/}). To debug these grammars, it is
useful, given a TAG tree, to see what its metagrammar origins are.  We
provide here an interface to the handy visualisation tool ViewTAG that
just does this.

\paragraph{runViewTag} runs Yannick Parmentier's ViewTAG module, which
displays trees produced by the XMG metagrammar system.  

\begin{code}
runViewTag :: ProgState -> String -> IO ()
runViewTag pst drName =  
  do -- figure out what grammar file to use
     let params  = pa pst
         gramfile = macrosFile params
     -- run the viewer 
     let cmd  = viewCmd params 
         args = [gramfile, drName]
     -- run the viewer
     runProcess cmd args Nothing Nothing Nothing Nothing Nothing
     return ()
\end{code}
