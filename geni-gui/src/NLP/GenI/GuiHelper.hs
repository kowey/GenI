-- GenI surface realiser
-- Copyright (C) 2005 Carlos Areces and Eric Kow
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module NLP.GenI.GuiHelper where

import Control.Applicative ( (<$>) )
import Control.Exception hiding ( bracket )
import Control.Monad ( unless, forM_ )
import Control.Monad.State.Strict ( execStateT, runState )
import Data.Array ( (!), listArray )
import Data.GraphViz.Exception ( GraphvizException(..) )
import Data.IORef
import Prelude hiding ( catch )
import System.Directory
import System.FilePath ((<.>),(</>),dropExtensions)
import System.Process (runProcess)
import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Text as T

import Graphics.UI.WX

import NLP.GenI
import NLP.GenI.Automaton (numStates, numTransitions)
import NLP.GenI.Builder (queryCounter, num_iterations, chart_size, num_comparisons)
import NLP.GenI.Configuration ( Params(..), MetricsFlg(..), setFlagP, getFlagP
                              , hasOpt, Optimisation(..)
                              , MacrosFlg(..), ViewCmdFlg(..) )
import NLP.GenI.General (dropTillIncluding, ePutStrLn)
import NLP.GenI.GeniParsers ( geniTagElems, parseFromFile )
import NLP.GenI.GeniShow(geniShow)
import NLP.GenI.Graphviz
import NLP.GenI.GraphvizShow (GvItem(..), gvItemSetFlag, gvItemLabel)
import NLP.GenI.GraphvizShowPolarity ()
import NLP.GenI.Lexicon
import NLP.GenI.Polarity (PolAut, AutDebug, suggestPolFeatures)
import NLP.GenI.Pretty
import NLP.GenI.Statistics
import NLP.GenI.Tags ( idname, mapBySem, TagElem(ttrace, tinterface), TagItem(tgIdName), tagLeaves )
import NLP.GenI.TreeSchemata ( showLexeme )
import NLP.GenI.Warnings
import qualified NLP.GenI.Builder as B

-- ----------------------------------------------------------------------
-- Types
-- ----------------------------------------------------------------------

data GraphvizStatus = GvError String
                    | GvNoSuchItem Int
                    | GvCached
                    | GvCreated FilePath
  deriving Show

-- ----------------------------------------------------------------------
-- Lexically selected items
-- ----------------------------------------------------------------------

-- | 'pauseOnLexGui' allows the user to see lexical selection only and either
--   dump it to file or read replace it by the contents of some other file
pauseOnLexGui :: Params
              -> Window a             -- ^ parent window
              -> [ILexEntry]          -- ^ lexically selected items (before anchoring)
              -> [TagElem]            -- ^ lexically selected items
              -> GeniWarnings         -- ^ lexical selection warnings
              -> Maybe ([TagElem] -> IO ()) -- ^ run when “begin” is clicked
              -> GvIO () (GvItem Bool TagElem)
pauseOnLexGui config f lexs xs warns mjob = do
    p <- panel f []
    candV <- varCreate xs
    (tb, ref, updater) <- candidateGui config p xs warns
    -- supplementary button bar
    let dispCmd = do
            putStr . unlines $ map geniShow lexs
        saveCmd = do
            cs <- varGet candV
            maybeSaveAsFile f . unlines $ map geniShow cs
        loadCmd = loadLex $ \cs -> do
            varSet candV cs
            setGvDrawables ref (sectionsBySem cs)
            updater
    --
    dispBt <- button p [ text := "Show lexical entries", on command := dispCmd ]
    saveBt <- button p [ text := "Save selection", on command := saveCmd ]
    loadBt <- button p [ text := "Load", on command := loadCmd ]
    nextBt <- button p [ text := "Begin" ]
    let disableBar = forM_ [ loadBt, nextBt ] $
            \w -> set w [ enabled := False ]
        continue job = do
            disableBar
            varGet candV >>= job
    case mjob of
        Nothing -> disableBar
        Just j  -> set nextBt [ on command := continue j ]
    let lay = fill $ container p $ column 5
              [ fill tb, hfill (vrule 1)
              , row 0 [ row 5 [ widget dispBt, widget saveBt, widget loadBt ]
                      , hfloatRight $ widget nextBt ] ]
    return (lay, ref, updater)
  where
    loadLex job = do
        fsel <- fileOpenDialog f False True
                    "Choose your file..."
                    anyFile "" ""
        flip maybeIO fsel $ \file -> do
            parsed <- parseFromFile geniTagElems file
            case parsed of
                Left err -> errorDialog f "" (show err)
                Right c  -> job c


-- | 'candidateGui' displays the lexically selected items, grouped by the
--   semantics they subsume.
candidateGui :: Params
             -> Window a
             -> [TagElem]
             -> GeniWarnings
             -> GvIO () (GvItem Bool TagElem)
candidateGui config f xs warns = do
    pouter <- panel f []
    split  <- splitterWindow pouter []
    p  <- panel split []
    (tb,gvRef,updater) <- tagViewerGui config p "lexically selected item" "candidates"
                          $ sectionsBySem xs
    let polFeats = "Polarity attributes detected: " ++
                   case suggestPolFeatures xs of
                     [] -> "None! :-("
                     fs -> T.unpack (T.unwords fs)
        addPolFeats fs = if hasOpt Polarised config then polFeats : fs else fs
        lexWarnings = concatMap showGeniWarning . fromGeniWarnings . sortWarnings $ warns
        warning = unlines $ filter (not .  null) (addPolFeats lexWarnings)
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
          (\s -> set ifaceLst [ items := map prettyStr $ tinterface s ])
    Monad.unless (null xs) $ do
        addGvHandler gvRef updateTrace
        addGvHandler gvRef updateIface
        -- first time run
        gvSt <- readIORef gvRef
        updateIface gvSt
        updateTrace gvSt
    --
    let layMain = fill $ row 2 [ fill tb, vfill laySide ]
    warningTxt <- textCtrl split [ text := warning ]
    let lay = fill . container pouter
            $ fill $ hsplit split 5 100 (widget warningTxt) (container p layMain)
    return (lay, gvRef, updater)

sectionsBySem :: (TagItem t) => [t] -> [GvItem Bool t]
sectionsBySem tsem =
    concatMap section sem
  where
    semmap   = mapBySem tsem
    sem      = Map.keys semmap
    --
    lookupTr k = Map.findWithDefault [] k semmap
    section  k = GvHeader header : map tlab (lookupTr k)
      where
        header = "___" ++ prettyStr k ++ "___"
        tlab t = GvItem (tgIdName t) False t

-- ----------------------------------------------------------------------
-- Polarity Automata
-- ----------------------------------------------------------------------

-- | A browser to see the automata constructed during the polarity optimisation
--   step.
polarityGui :: Window a   -- ^ parent window
            -> [AutDebug] -- ^ intermediary automata
            -> PolAut     -- ^ final automaton
            -> GvIO () (GvItem () PolAut)
polarityGui   f xs final = do
    gvRef   <- newGvRef () "automata"
    setGvDrawables gvRef $ concatMap toItem xs ++ [finalItem]
    graphvizGui f "polarity" gvRef
 where
    toItem (pkey, a1, a2) = [ it (show pkey) a1
                            , it (show pkey ++ " pruned") a2 ]
    finalItem = it "final" final
    it  n a = GvItem (lab n a) () a
    lab n a = n ++ " (" ++ show (numStates a) ++ "st "
                        ++ show (numTransitions a) ++ "tr)"

-- ----------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------

-- | Any data structure which has corresponds to a TAG tree and which
--   has some notion of derivation
class XMGDerivation a where
    getSourceTrees :: a -> [String]

instance XMGDerivation TagElem where
    getSourceTrees te = [idname te]

-- | 'toSentence' almost displays a 'TagElem' as a sentence, but only good
-- enough for debugging needs.  The problem is that each leaf may be
-- an atomic disjunction. Our solution is just to display each choice and
-- use some delimiter to seperate them.  We also do not do any
-- morphological processing.
toSentence :: TagElem -> String
toSentence = unwords . map squishLeaf . tagLeaves

squishLeaf :: (a,([String], b)) -> String
squishLeaf = showLexeme . fst .snd

-- ----------------------------------------------------------------------
-- TAG viewer
-- ----------------------------------------------------------------------

-- | Variant of 'graphvizGui' with a toggle to view feature structures
tagViewerGui :: (GraphvizShow (GvItem Bool t), XMGDerivation t)
             => Params
             -> Window a        -- ^ parent
             -> String          -- ^ tooltip
             -> FilePath        -- ^ cache directory for graphviz
             -> [GvItem Bool t] -- ^ items
             -> GvIO () (GvItem Bool t)
tagViewerGui config f tip cachedir itNlab = do
    p <- panel f []
    gvRef <- newGvRef () tip
    setGvDrawables gvRef itNlab
    (lay,ref,onUpdate) <- graphvizGui p cachedir gvRef
    -- button bar widgets
    detailsChk <- checkBox p [ text := "Show features"
                             , checked := False ]
    viewTagLay <- viewTagWidgets p gvRef config
    -- handlers
    let onDetailsChk = do
            isDetailed <- get detailsChk checked
            modifyGvItems gvRef (gvItemSetFlag isDetailed)
            onUpdate
    set detailsChk [ on command := onDetailsChk ]
    -- pack it all in
    let cmdBar = hfill $ row 5 [ dynamic $ widget detailsChk, viewTagLay ]
        lay2   = fill  $ container p $ column 5 [ fill lay, cmdBar ]
    return (lay2,ref,onUpdate)

-- ----------------------------------------------------------------------
-- XMG Metagrammar stuff.
-- See <http://sourcesup.cru.fr/xmg/>
-- ----------------------------------------------------------------------

-- | Calls Yannick Parmentier's handy visualisation tool ViewTAG.
viewTagWidgets :: (GraphvizShow (GvItem Bool t), XMGDerivation t)
               => Window a -- ^ parent window
               -> GraphvizGuiRef st (GvItem Bool t)
               -> Params
               -> IO Layout
viewTagWidgets p gvRef config = do
    viewTagBtn <- button p [ text := "ViewTAG" ]
    viewTagCom <- choice p [ tooltip := "derivation tree" ]
    -- handlers
    let onViewTag d = readIORef gvRef >>=
         gvOnSelect (return ()) (const (runViewTag config d))
    -- when the user selects a tree, we want to update the list of derivations
    let updateDerivationList = gvOnSelect
          (set viewTagCom [ enabled := False ])
          (\s -> do
               let dervs = getSourceTrees s
               set viewTagCom [ enabled := True, items := dervs ]
               setSelection viewTagCom dervs 0 $ \d ->
                   set viewTagBtn [ on command := onViewTag d ]
          )
    addGvHandler gvRef updateDerivationList
    updateDerivationList =<< readIORef gvRef
    --
    return $ row 5 $ map dynamic [ widget viewTagCom, widget viewTagBtn ]

runViewTag :: Params -> String -> IO ()
runViewTag params drName =
    maybeOrWarn warnTf (getFlagP MacrosFlg params)  $ \f ->
    maybeOrWarn warnVc (getFlagP ViewCmdFlg params) $ \cmd ->
       actuallyView f cmd
  where
    warnTf = "Warning: No tree schema file specified (runViewTag)"
    warnVc = "Warning: No viewcmd specified (runViewTag)"
    treenameOnly = takeWhile (/= ':')
                 . dropTillIncluding ':' . dropTillIncluding ':'
    -- run the viewer
    actuallyView f cmd =
        runProcess cmd [ dropExtensions f <.> "rec", treenameOnly drName]
           Nothing Nothing Nothing Nothing Nothing >> return ()

-- --------------------------------------------------------------------
-- Graphical debugger (helper functions)
-- --------------------------------------------------------------------


type DebuggerItemBar st flg itm =
       Panel ()                           -- ^ parent panel
    -> GraphvizGuiRef st (GvItem flg itm) -- ^ gv ref to use
    -> GvUpdater -- ^ onUpdate
    -> IO (Layout, GvUpdater)

data GraphvizShow (GvItem flg itm) => Debugger st flg itm = Debugger
    { dBuilder :: B.Builder st itm Params
      -- ^ builder to use
    , dToGv :: st -> [GvItem flg itm]
      -- ^ function to convert a Builder state into lists of items
      --   and their labels, the way graphvizGui likes it
    , dControlPnl :: DebuggerItemBar st flg itm
      -- ^ function returning a control panel configuring
      --   how you want the currently selected item in the debugger
      --   to be displayed
    , dCacheDir :: FilePath
      -- ^ graphviz cache directory
    , dNext     :: [GeniResult] -> Statistics -> IO ()
    }

-- | A generic graphical debugger widget for GenI, including
--
--   * item viewer which allows the user to select one of the items in the
--     builder state.
--
--   * item bar which provides some options on how to view the currently
--     selected item, for example, if you want to display the features or not.
--
--   * A dashboard which lets the user do things like ``go ahead 6 steps''.
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
debuggerPanel :: GraphvizShow (GvItem flg itm)
              => Debugger st flg itm
              -> ProgStateRef
              -> Window a -- ^ parent window
              -> B.Input
              -> IO Layout
debuggerPanel (Debugger {..}) pstRef f input = do
    config <- (setMetrics . pa) <$> readIORef pstRef
    let (initS, initStats) = initBuilder input config
    p <- panel f []
    -- ---------------------------------------------------------
    -- item viewer: select and display an item
    -- ---------------------------------------------------------
    gvRef <- newGvRef initS "debugger session"
    setGvDrawables gvRef (dToGv initS)
    (layItemViewer,_,onUpdateMain) <- graphvizGui p dCacheDir gvRef
    -- ----------------------------------------------------------
    -- item bar: controls for how an individual item is displayed
    -- ----------------------------------------------------------
    (layItemBar,onUpdateItemBar) <- dControlPnl p gvRef onUpdateMain
    -- -------------------------------------------
    -- dashboard: controls for the debugger itself
    -- -------------------------------------------
    let onUpdate = onUpdateMain >> onUpdateItemBar
    db <- panel p []
    restartBt <- button db [text := "Start over"]
    nextBt    <- button db [text := "Step by..."]
    leapVal   <- entry  db [ text := "1", clientSize := sz 30 25 ]
    finishBt  <- button db [text := "Leap to end"]
    statsTxt  <- staticText db []
    done      <- varCreate False
    -- dashboard commands
    let showQuery c gs = maybe "???" show (queryCounter c gs)
        updateStatsTxt gs = set statsTxt [ text :~ (\_ -> txtStats gs) ]
        txtStats   gs =  unwords [ "itr",  showQuery num_iterations gs
                                 , "chart sz", showQuery chart_size gs
                                 ]
                      ++ "\ncomparisons: " ++ showQuery num_comparisons gs
    let genStep _ (st,stats) = runState (execStateT nextStep st) stats
    let showNext s_stats = do
            leapTxt <- get leapVal text
            let leapInt :: Integer
                leapInt = read leapTxt
                (s2,stats2) = foldr genStep s_stats [1..leapInt]
            modifyIORef gvRef $ \g -> g { gvcore = s2 }
            setGvDrawables gvRef (dToGv s2)
            setGvSel gvRef 1
            onUpdate
            updateStatsTxt stats2
            set nextBt [ on command :~ (\_ -> showNext (s2,stats2) ) ]
            case B.finished dBuilder s2 of
                B.Active -> return ()
                _        -> callNext done stats2 s2
    let showLast = do
             -- redo generation from scratch
             let (s2, stats2) = runState (execStateT allSteps initS) initStats
             setGvDrawables gvRef (dToGv s2)
             onUpdate
             updateStatsTxt stats2
             callNext done stats2 s2
    let showReset = do
             set nextBt   [ on command  := showNext (initS, initStats) ]
             updateStatsTxt initStats
             setGvDrawables gvRef (dToGv initS)
             setGvSel gvRef 1
             onUpdate
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
  where
    initBuilder = B.init  dBuilder
    nextStep    = B.step  dBuilder
    allSteps    = B.stepAll dBuilder
    setMetrics  = setFlagP MetricsFlg B.defaultMetricNames
    -- builder stateToGv itemBar f config_ input cachedir = do
    callNext d stats st = do
        done <- varGet d
        unless done $ do
            varSet d True
            results <- extractResults pstRef dBuilder st
            dNext results stats

-- --------------------------------------------------------------------
-- Graphviz GUI
-- --------------------------------------------------------------------


data GraphvizOrder = GvoParams | GvoItems | GvoSel
  deriving Eq

data GraphvizGuiSt st a = GvSt
    { gvcore    :: st
    , gvitems   :: Map.Map Int a
    , gvlabels  :: [String]
    , gvtip     :: String -- ^ tooltip for the selection box
    -- | handler function to call when the selection is
    --   updated (note: before displaying the object)
    , gvhandler :: Maybe (GraphvizGuiSt st a -> IO ())
    , gvsel     :: Int
    , gvorders  :: [GraphvizOrder]
    }

-- | This provides a mechanism for communicating with the GUI.  The basic idea:
--
--  1. you create a GvRef with newGvRef
--
--  2. you call 'graphvizGui' and get back an updater function
--
--  3. whenever you want to modify something, you use setGvWhatever and call
--     the updater function
--
--  4. if you want to react to the selection being changed, you should set
--     gvhandler
type GraphvizGuiRef st a = IORef (GraphvizGuiSt st a)

newGvRef :: st -> String -> IO (GraphvizGuiRef st a)
newGvRef initSt t = newIORef GvSt
    { gvcore = initSt
    , gvitems  = Map.empty
    , gvlabels  = []
    , gvhandler = Nothing
    , gvtip    = t
    , gvsel    = 0
    , gvorders = []
    }

setGvSel :: GraphvizGuiRef st a -> Int -> IO ()
setGvSel gvref s  = modifyIORef gvref $ \x ->
    x { gvsel = s, gvorders = GvoSel : gvorders x }

modifyGvItems :: GraphvizGuiRef st a -> (a -> a) -> IO ()
modifyGvItems gvref fn =
    modifyIORef gvref $ \s -> s { gvitems = Map.map fn (gvitems s) }

setGvDrawables :: GraphvizGuiRef st (GvItem f a) -> [GvItem f a] -> IO ()
setGvDrawables gvref itlb = modifyIORef gvref $ \x ->
    x { gvitems  = Map.fromList $ zip [0..] itlb
      , gvlabels = map gvItemLabel itlb
      , gvorders = GvoItems : gvorders x
      }

-- | Helper function for making selection handlers (see 'addGvHandler')
--   Note that this was designed for cases where the contents is a Maybe
gvOnSelect :: IO () -> (a -> IO ()) -> GraphvizGuiSt st (GvItem f a) -> IO ()
gvOnSelect onNothing onJust gvSt =
    case Map.lookup sel things of
      Just (GvItem _  _ s) -> onJust s
      _                    -> onNothing
  where
    sel    = gvsel gvSt
    things = gvitems gvSt

setGvHandler :: GraphvizGuiRef st a -> Maybe (GraphvizGuiSt st a -> IO ()) -> IO ()
setGvHandler gvref mh = do
    gvSt <- readIORef gvref
    modifyIORef gvref (\x -> x { gvhandler = mh })
    maybeIO ($ gvSt) mh

-- | add a selection handler - if there already is a handler
--   this handler will be called before the new one
addGvHandler :: GraphvizGuiRef st a -> (GraphvizGuiSt st a -> IO ()) -> IO ()
addGvHandler gvref h = do
    gvSt <- readIORef gvref
    let newH = case gvhandler gvSt of
                 Nothing   -> h
                 Just oldH -> \g -> oldH g >> h g
    setGvHandler gvref (Just newH)

type GvIO st d  = IO (Layout, GraphvizGuiRef st d, GvUpdater)
type GvUpdater = IO ()

-- |'graphvizGui' @f glab cachedir gvRef@ is a general-purpose GUI for
-- displaying a list of items graphically via AT&T's excellent Graphviz
-- utility.  We have a list box where we display all the labels the user
-- provided.  If the user selects an entry from this box, then the item
-- corresponding to that label will be displayed.
--
-- This returns a layout (wxhaskell container) and a function that you're
-- expected to call whever something changes that would require the GUI to
-- refresh itself (for example, you create a new chart item)
--
--  * @f@ - (parent window) the GUI is provided as a panel within the parent.
--    Note: we use window in the WxWidget's sense, meaning it could be
--    anything as simple as a another panel, or a notebook tab.
--  * @glab@ - (gui labels) a tuple of strings (tooltip, next button text)
--  * @cachedir@ - the cache subdirectory.  We intialise this by creating a cache
--    directory for images which will be generated from the results
--  * @gvRef@ - see above
graphvizGui :: GraphvizShow d => Window a -> String -> GraphvizGuiRef st d -> GvIO st d
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
    ------------------------------------------------
    -- create an updater function
    ------------------------------------------------
    let withoutSelector job =
          bracket ( swap rchoice (on select) (return ()) )
                  ( \fn -> set rchoice [ on select := fn ] )
                  ( const job )
        -- the selector calls onUpdate which calls the selector
        -- indirectly by setting the selection
    let onUpdate = withoutSelector $ do
          gvSt <- readIORef gvRef
          let orders = gvorders gvSt
          initCacheDir cachedir
          Monad.when (GvoItems `elem` orders) $
            set rchoice [ items := gvlabels gvSt ]
          Monad.when (GvoSel `elem` orders) $
            set rchoice [ selection := gvsel gvSt ]
          modifyIORef gvRef (\x -> x { gvorders = []})
          createAndOpenImage cachedir p gvRef openFn
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
          maybeIO ($ gvSt) (gvhandler gvSt)
          -- now do the update
          onUpdate
    ------------------------------------------------
    set rchoice [ on select := selectAndShow ]
    -- call the updater function for the first time
    setGvSel gvRef 1
    onUpdate
    -- return the layout, the gvRef, and an updater function
    -- The gvRef is to make it easier for users to muck around with the
    -- state of the gui.  Here, it's trivial, but when people combine guis
    -- together, it might be easier to keep track of when returned
    return (lay, gvRef, onUpdate)

-- ----------------------------------------------------------------------
-- Bitmap stuff
-- ----------------------------------------------------------------------

-- | Bitmap with a scrollbar
scrolledBitmap :: Window a -> IO(VarBitmap, ScrolledWindow ())
scrolledBitmap p = do
    dtBitmap <- variable [value := Nothing]
    sw       <- scrolledWindow p [ scrollRate := sz 10 10
                                 , bgcolor := white
                                 , on paint := onPaint dtBitmap
                                 , fullRepaintOnResize := False
                                 ]
    return (dtBitmap, sw)

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

closeImage :: VarBitmap -> IO ()
closeImage vbitmap =
    maybeIO objectDelete =<< swap vbitmap value Nothing

onPaint :: VarBitmap -> DC a -> b -> IO ()
onPaint vbitmap dc _ =
    maybeIO draw =<< get vbitmap value
  where
    draw bm = dcClear dc >> drawBitmap dc bm pointZero False []

-- | 'createAndOpenImage' attempts to draw an image (or retrieve it from cache)
-- and opens it if we succeed.  Otherwise, it does nothing at all; the creation
-- function will display an error message if it fails.
createAndOpenImage :: GraphvizShow b
                   => FilePath  -- ^ cache directory
                   -> Window a  -- ^ parent window
                   -> GraphvizGuiRef st b
                   -> OpenImageFn
                   -> IO ()
createAndOpenImage cachedir f gvref openFn = do
    gvStatus <- createImage cachedir f gvref
    case gvStatus of
      GvCreated g    -> openGraphic g
      GvNoSuchItem _ -> return ()
      GvError err    -> errorDialog f "" err
      GvCached       -> return ()
  where
    openGraphic graphic = do
        exists <- doesFileExist graphic
        if exists
           then openFn graphic
           else errorDialog f "" (noFile graphic)
    noFile g = "The file " ++ g ++ " was not created! Is graphviz installed?"

-- | Creates a graphical visualisation for anything which can be displayed
--   by graphviz.
createImage :: GraphvizShow b
            => FilePath            -- ^ cache directory
            -> Window a            -- ^ parent window
            -> GraphvizGuiRef st b -- ^ stuff to display
            -> IO GraphvizStatus
createImage cachedir f gvref = do
    gvSt <- readIORef gvref
    -- putStrLn $ "creating image via graphviz"
    let drawables = gvitems  gvSt
        sel       = gvsel    gvSt
    dotFile <- createDotPath cachedir (show sel)
    graphicFile <-  createImagePath cachedir (show sel)
    let create x = do _ <- toGraphviz x dotFile graphicFile
                      return . GvCreated $ graphicFile
        handler :: GraphvizException -> IO GraphvizStatus
        handler err = do
            errorDialog f "Error calling graphviz. Is it installed?" (show err)
            return . GvError . show $ err
    exists <- doesFileExist graphicFile
    -- we only call graphviz if the image is not in the cache
    if exists
       then return (GvCreated graphicFile)
       else case Map.lookup sel drawables of
              Nothing -> return (GvNoSuchItem sel)
              Just it -> create it `catch` handler

-- | Directory to dump image files in so that we can avoid regenerating them.
--   If the directory already exists, we can just delete all the files in it.
initCacheDir :: String -> IO()
initCacheDir cachesubdir = do
    mainCacheDir <- gvCACHEDIR
    cmainExists  <- doesDirectoryExist mainCacheDir
    unless cmainExists $ createDirectory mainCacheDir
    let cachedir = mainCacheDir </> cachesubdir
    cExists <- doesDirectoryExist cachedir
    if cExists
       then do
           contents <- filter notJunk <$> getDirectoryContents cachedir
           mapM_ (removeFile . (cachedir </>)) contents
       else createDirectory cachedir
  where
    notJunk = (`notElem` [".", ".."])

-- ----------------------------------------------------------------------
-- Miscellaneous
-- ----------------------------------------------------------------------

-- | Set a selection widget's selection reactor
--   We assume you've already populated it (radio boxes cannot be added to,
--   so we have to let you do it manually on initialisation)
setSelection :: (Selecting w, Selection w, Items w String)
             => w                    -- ^ widget
             -> [a]                  -- ^ items
             -> Int                  -- ^ initial selection
             -> (a -> IO ())         -- ^ on selection
             -> IO ()
setSelection widgt xs initial reactor = do
    set widgt [ selection := initial
              , on select := onSelection ]
    onSelection
  where
    imap = listArray (0, length xs - 1) xs
    onSelection = do
        sel <- get widgt selection
        reactor (imap ! sel)

-- | Save the given string to a file, if the user selets one via the file save
--   dialog. Otherwise, don't do anything.
maybeSaveAsFile :: Window a -> String -> IO ()
maybeSaveAsFile f msg = do
    fsel <- fileSaveDialog f False True "Save to" anyFile "" ""
    case fsel of
      Nothing   -> return ()
      Just file -> writeFile file msg

-- | A message panel for use by the Results gui panels.
messageGui :: Window a -> String -> IO Layout
messageGui f msg = do
    p <- panel f []
    -- sw <- scrolledWindow p [scrollRate := sz 10 10 ]
    t  <- textCtrl p [ text := msg, enabled := False ]
    return $ fill $ container p $ column 1 [ fill (widget t) ]

gvCACHEDIR :: IO String
gvCACHEDIR = do
    home <- getHomeDirectory
    return (home </> ".gvcache")

createImagePath :: String -> String -> IO String
createImagePath subdir name = do
    cdir <- gvCACHEDIR
    return (cdir </> subdir </> name <.> "png")

createDotPath :: String -> String -> IO String
createDotPath subdir name = do
    cdir <- gvCACHEDIR
    return (cdir </> subdir </> name <.> "dot")

maybeOrWarn :: String  -- ^ warning
            -> Maybe a
            -> (a -> IO ())
            -> IO ()
maybeOrWarn warning m job = maybe (ePutStrLn warning) job m

maybeIO :: (a -> IO ()) -> Maybe a -> IO ()
maybeIO = maybe (return ())

anyFile :: [ (String,[String]) ]
anyFile = [("Any file",["*","*.*"])]
