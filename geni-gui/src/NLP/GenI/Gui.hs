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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NLP.GenI.Gui (guiGeni) where

import Control.Applicative ( (<$>) )
import Control.Exception ( catch, try, SomeException )
import Control.Monad ( unless )
import Data.IORef ( readIORef, modifyIORef )
import Data.List ( nub, delete, findIndex)
import Data.Maybe ( fromMaybe, catMaybes )
import Data.Version ( showVersion )
import Prelude hiding ( catch )
import System.Directory
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.FilePath ( makeRelative )

import Graphics.UI.WX

import NLP.GenI
    ( ProgState(..), ProgStateRef, initGeni , GeniResult(..), prettyResult
    , parseSemInput, loadEverything, loadTestSuite
    )
import NLP.GenI.Configuration
    ( Params(..), Instruction, hasOpt , hasFlagP
    , deleteFlagP, setFlagP, getFlagP, getListFlagP
    , parseFlagWithParsec
    --
    , DetectPolaritiesFlg(..) , ExtraPolaritiesFlg(..), LexiconFlg(..)
    , MacrosFlg(..) , MorphCmdFlg(..) , MorphInfoFlg(..), OptimisationsFlg(..)
    , RankingConstraintsFlg(..) , RootFeatureFlg(..) , TestCaseFlg(..)
    , TestSuiteFlg(..) , TestInstructionsFlg(..) , ViewCmdFlg(..)
    --
    , Optimisation(..) , BuilderType(..), mainBuilderTypes
    )
import NLP.GenI.General (fst3, prettyException, trim)
import NLP.GenI.GeniParsers hiding ( choice, label, tab, try )
import NLP.GenI.GeniShow (geniShow)
import NLP.GenI.GuiHelper
import NLP.GenI.Polarity
import NLP.GenI.Pretty
import NLP.GenI.Semantics
import NLP.GenI.Simple.SimpleGui
import NLP.GenI.TestSuite ( TestCase(..) )
import Paths_geni_gui ( version )
import qualified NLP.GenI.Builder as B
import qualified NLP.GenI.BuilderGui as BG

-- Main Gui

guiGeni :: ProgStateRef -> IO()
guiGeni pstRef = start (mainGui pstRef)

mainGui :: ProgStateRef -> IO ()
mainGui pstRef = do
    pst <- readIORef pstRef
    -- Top Window
    f <- frame [text := "Geni Project"]
    -- create statusbar field
    status <- statusField   []
    -- create the file menu
    fileMen   <- menuPane [text := "&File"]
    loadMenIt <- menuItem fileMen [text := "&Open files or configure GenI"]
    quitMenIt <- menuQuit fileMen [text := "&Quit"]
    set quitMenIt [on command := close f ]
    -- create the help menu
    helpMen   <- menuPane [text := "&Help"]
    aboutMeIt <- menuAbout helpMen [help := "About"]
    -- Tie the menu to this window
    set f [ statusBar := [status]
          , menuBar := [fileMen, helpMen]
          -- put the menu event handler for an about box on the frame.
          , on (menu aboutMeIt) := infoDialog f "About GenI"
             ("The GenI generator " ++ showVersion version ++
              ".\nhttp://projects.haskell.org/GenI" )
          -- event handler for the tree browser
          -- , on (menu gbrowserMenIt) := do { loadEverything pstRef; treeBrowserGui pstRef }
          ]
    -- -----------------------------------------------------------------
    -- buttons
    -- -----------------------------------------------------------------
    let config     = pa pst
        hasSem     = hasFlagP TestSuiteFlg config
    -- Target Semantics
    testSuiteChoice <- choice f [ selection := 0, enabled := hasSem ]
    tsTextBox <- textCtrl f [ wrap := WrapWord
                            , clientSize := sz 400 80
                            , enabled := hasSem
                            , text := "" ]
    testCaseChoice <- choice f [ selection := 0
                               , enabled := hasSem ]
    -- Detect polarities and root feature
    let initialDP = maybe "" showPolarityAttrs (getFlagP DetectPolaritiesFlg config)
        initialRF = maybe "" prettyStr (getFlagP RootFeatureFlg config)
    detectPolsTxt <- entry f [ text := initialDP ]
    rootFeatTxt   <- entry f [ text := initialRF ]
    -- Box and Frame for files loaded
    macrosFileLabel  <- staticText f [ text := getListFlagP MacrosFlg config  ]
    lexiconFileLabel <- staticText f [ text := getListFlagP LexiconFlg config ]
    -- Generate and Debug
    let genfn = doGenerate f pstRef tsTextBox detectPolsTxt rootFeatTxt
    pauseOnLexChk <- checkBox f [ text := "Inspect lex", tooltip := "Affects debugger only"  ]
    debugBt <- button f [ text := "Debug"
                        , on command := get pauseOnLexChk checked >>= genfn True ]
    genBt  <- button f  [text := "Generate", on command := genfn False False ]
    quitBt <- button f  [ text := "Quit",
              on command := close f]
    -- -----------------------------------------------------------------
    -- optimisations
    -- -----------------------------------------------------------------
    let setBuilder b = modifyIORef pstRef . modifyParams
                     $ \p -> p { builderType = b }
        initialSelection = case builderType config of
                             SimpleBuilder         -> 0
                             SimpleOnePhaseBuilder -> 1
    algoChoiceBox <- radioBox f Vertical (map show mainBuilderTypes) []
    setSelection algoChoiceBox mainBuilderTypes initialSelection setBuilder
    polChk <- optCheckBox f pstRef polarisedBio
    useSemConstraintsChk <- optCheckBox f pstRef semConstraintBio
    extrapolText <- staticText f
        [ text := maybe "" showLitePm $ getFlagP ExtraPolaritiesFlg config
        , tooltip := "Use the following additional polarities"
        ]
    -- commands for the checkboxes
    let togglePolStuff = do
            c <- get polChk checked
            set extrapolText [ enabled := c ]
    set polChk [on command :~ (>> togglePolStuff) ]
    -- -----------------------------------------------------------------
    -- layout; packing it all together
    -- -----------------------------------------------------------------
    -- set any last minute handlers, run any last minute functions
    let myWidgets = MainWidgets
            { f = f
            , macrosFileLabel  = macrosFileLabel
            , lexiconFileLabel = lexiconFileLabel
            , testSuiteChoice  = testSuiteChoice
            , testCaseChoice   = testCaseChoice
            , tsTextBox        = tsTextBox
            }
        onLoad = mainOnLoad pstRef myWidgets
    set loadMenIt [ on command := configGui pstRef onLoad ]
    onLoad
    togglePolStuff
    --
    let labeledRow l w = row 1 [ label l, hfill (widget w) ]
    let gramsemBox = boxed "Files last loaded" $
                hfill $ column 1
                  [ labeledRow "trees:"   macrosFileLabel
                  , labeledRow "lexicon:" lexiconFileLabel
                  ]
        optimBox =  --boxed "Optimisations " $ -- can't used boxed with wxwidgets 2.6 -- bug?
                 column 5 [ label "Algorithm"
                          , dynamic $ widget algoChoiceBox
                          , label "Optimisations"
                          , dynamic $ widget polChk
                          , row 5 [ label "  ", column 5
                                  [ dynamic $ row 5 [ label "Extra: ", widget extrapolText ] ] ]
                          , dynamic $ widget useSemConstraintsChk
                          ]
    set f [layout := column 5 [ gramsemBox
                , row 5 [ fill $ -- boxed "Input Semantics" $
                          hfill $ column 5
                            [ labeledRow "test suite: " testSuiteChoice
                            , labeledRow "test case: "  testCaseChoice
                            , fill  $ widget tsTextBox
                            , row 1 [ label "detect pols: "
                                    , hfill (widget detectPolsTxt)
                                    , glue
                                    , label "root feature: "
                                    , hfill (widget rootFeatTxt)
                                    ]
                            ]
                        , vfill optimBox ]
                 -- ----------------------------- Generate and quit
                , row 1 [ widget quitBt
                       , hfloatRight $ row 5 [ widget pauseOnLexChk, widget debugBt, widget genBt ]] ]
         -- , clientSize := sz 625 325
         , on closing := exitWith ExitSuccess
         ]

data MainWidgets = MainWidgets
    { f                :: Frame ()
    , macrosFileLabel  :: StaticText ()
    , lexiconFileLabel :: StaticText ()
    , testSuiteChoice  :: Choice ()
    , testCaseChoice   :: Choice ()
    , tsTextBox        :: TextCtrl ()
    }

mainOnLoad :: ProgStateRef -> MainWidgets -> IO ()
mainOnLoad pstRef (MainWidgets {..}) = do
    cfg <- pa `fmap` readIORef pstRef -- we want the latest config!
    -- errHandler title err = errorDialog f title (show err)
    set macrosFileLabel  [ text := getListFlagP MacrosFlg cfg ]
    set lexiconFileLabel [ text := getListFlagP LexiconFlg cfg ]
    -- read the test suite if there is one
    case getListFlagP TestInstructionsFlg cfg of
      [] -> do
          set testSuiteChoice [ enabled := False, items := [] ]
          set testCaseChoice  [ enabled := False, items := [] ]
      is -> do
          set testSuiteChoice [ enabled := True,  items := map fst is ]
          setSelection testSuiteChoice is 0 $
             \t -> loadTestSuiteAndRefresh f pstRef t (tsTextBox, testCaseChoice)

-- ----------------------------------------------------------------------
-- Toggling optimisations
-- ----------------------------------------------------------------------

-- | optimisation or pessimisation?
data OptType = Opti | Pessi

data OptBio = OptBio
    { odType     :: OptType
    , odOpt      :: Optimisation
    , odShortTxt :: String -- ^ confusing detail: always describes an optimisation
                           --   (so it says the straightforward thing for optimisations
                           --    but the opposite meaning for pessimisations)
    , odToolTip  :: String -- ^ see confusing detail above
    }

polarisedBio :: OptBio
polarisedBio = OptBio Opti Polarised
    "Polarities"
    "Use the polarity optimisation"

semConstraintBio :: OptBio
semConstraintBio = OptBio Pessi NoConstraints
    "Sem constraints"
    "Use any sem constraints the user provides"

optBios :: [OptBio]
optBios = [ polarisedBio, semConstraintBio ]

-- | Note the following point about pessimisations: An pessimisation
--   disables a default behaviour which is assumed to be "optimisation".  But of
--   course we don't want to confuse the GUI user, so we confuse the programmer
--   instead: Given an pessimisation DisableFoo, we have a check box UseFoo.  If
--   UseFoo is checked, we remove DisableFoo from the list; if it is unchecked, we
--   add it to the list.  This is the opposite of the default behaviour, but the
--   result, I hope, is intuitive for the user.
optCheckBox :: Window a -> ProgStateRef -> OptBio -> IO (CheckBox ())
optCheckBox f pstRef od = do
    config <- pa <$> readIORef pstRef
    chk <- checkBox f [ checked := flippy (hasOpt o config)
                      , text    := odShortTxt od
                      , tooltip := odToolTip od
                      ]
    set chk [ on command := onCheck chk ]
    return chk
  where
    o = odOpt od
    flippy = case odType od of
               Opti  -> id
               Pessi -> not
    onCheck chk = do
        isChecked <- get chk checked
        config    <- pa <$> readIORef pstRef
        let modopt  = if flippy isChecked then (o:) else delete o
            newopts = nub . modopt $ getListFlagP OptimisationsFlg config
        modifyIORef pstRef . modifyParams $ setFlagP OptimisationsFlg newopts

-- --------------------------------------------------------------------
-- Loading files
-- --------------------------------------------------------------------

-- | Load the given test suite and update the GUI accordingly.
--   This is used when you first start the graphical interface
--   or when you run the configuration menu.
loadTestSuiteAndRefresh :: (Textual a, Selecting b, Selection b, Items b String)
              => Window w
              -> ProgStateRef
              -> Instruction
              -> (a, b) -- ^ test suite text and case selector widgets
              -> IO ()
loadTestSuiteAndRefresh f pstRef (suitePath,mcs) widgets = do
    pst    <- readIORef pstRef
    msuite <- try (loadTestSuite pstRef)
    let mcase = getFlagP TestCaseFlg (pa pst)
    case msuite of
      Left e  -> errorDialog f ("Error reading test suite " ++ suitePath) $ show (e :: SomeException)
      Right s -> onTestSuiteLoaded f s mcs mcase widgets

-- | Helper for 'loadTestSuiteAndRefresh'
onTestSuiteLoaded :: (Textual a, Selecting b, Selection b, Items b String)
                  => Window w
                  -> [TestCase]     -- ^ loaded suite
                  -> Maybe [String] -- ^ subset of test cases to select (instructions)
                  -> Maybe String   -- ^ particular test case to focus on
                  -> (a, b) -- ^ test suite text and case selector widgets
                  -> IO ()
onTestSuiteLoaded f suite mcs mcase (tsBox, caseChoice) = do
    -- if the instructions specify a set of cases, we hide the cases that aren't mentioned
    let suiteCases = case filter (\c -> tcName c `elem` fromMaybe [] mcs) suite of
                       []  -> suite
                       res -> res
    -- handler for selecting a test case
    unless (null suiteCases) $ do
        initial <- getInitialSelection mcase suiteCases
        set caseChoice [ items := zipWith numfn [1..] suiteCases ]
        setSelection caseChoice suiteCases initial setTsBox
  where
    -- we number the cases for easy identification, putting
    -- a star to highlight the selected test case (if available)
    numfn :: Int -> TestCase -> String
    numfn n t = concat [ if hasName (fromMaybe "" mcase) t then "* " else ""
                       , show  n
                       , ". "
                       , tcName t
                       ]
    -- first case selected is either specified
    getInitialSelection Nothing  _      = return 0
    getInitialSelection (Just n) tcases =
        case findIndex (hasName n) tcases of
          Nothing -> do errorDialog f "" ("No such test case: " ++ n)
                        return 0
          Just i  -> return i
    hasName name tc = tcName tc == name
    --
    setTsBox (TestCase {..}) =
        set tsBox [ text := geniShow (toSemInputString tcSem tcSemString) ]

-- --------------------------------------------------------------------
-- Configuration
-- --------------------------------------------------------------------

-- | 'configGui' @pstRef loadFn@ provides the configuration GUI. The continuation
--   @loadFn@ tells us what to do when the user closes this window.
configGui ::  ProgStateRef -> IO () -> IO ()
configGui pstRef loadFn = do
    pst <- readIORef pstRef
    let config = pa pst
    --
    f  <- frame []
    p  <- panel f []
    nb <- notebook p []
    let browseTxt = "Browse"
    --
    let fakeBoxed title lst = hstretch $ column 3 $ map hfill $
          [ hrule 1 , alignRight $ label title, vspace 5 ]
          ++ map hfill lst
    let longSize  = sz 20 (25 :: Int)
    -- -----------------------------------------------------------------
    -- basic options tab
    -- -----------------------------------------------------------------
    pbas <- panel nb []
    -- files loaded (labels)
    macrosFileLabel  <- staticText pbas [ text := getListFlagP MacrosFlg config  ]
    lexiconFileLabel <- staticText pbas [ text := getListFlagP LexiconFlg config ]
    tsFileLabel      <- staticText pbas [ text := getListFlagP TestSuiteFlg config ]
    -- "Browse buttons"
    macrosBrowseBt  <- button pbas [ text := browseTxt ]
    lexiconBrowseBt <- button pbas [ text := browseTxt ]
    tsBrowseBt      <- button pbas [ text := browseTxt ]
    -- root feature
    detectPolsTxt <- entry pbas
        [ text := maybe "" showPolarityAttrs
                      (getFlagP DetectPolaritiesFlg config)
        , size := longSize ]
    rootFeatTxt <- entry pbas
        [ text := maybe "" prettyStr (getFlagP RootFeatureFlg config)
        , size := longSize ]
    let layFiles = [ row 1 [ label "trees:"
                           , fill $ widget macrosFileLabel
                           , widget macrosBrowseBt  ]
                   , row 1 [ label "lexicon:"
                           , fill $ widget lexiconFileLabel
                           , widget lexiconBrowseBt ]
                   , row 1 [ label "test suite:"
                           , fill $ widget tsFileLabel
                           , widget tsBrowseBt ]
                   , hspace 5
                   , hfill $ vrule 1
                   , row 3 [ label "detect polarities"
                           , hglue
                           , rigid $ widget detectPolsTxt ]
                   , row 3 [ label "root features"
                           , hglue
                           , rigid $ widget rootFeatTxt ]
                   ]
      -- the layout for the basic stuff
    let layBasic = dynamic $ container pbas $ -- boxed "Basic options" $
            hfloatLeft $ dynamic $ fill $ column 4 $ map (dynamic.hfill) layFiles
    -- -----------------------------------------------------------------
    -- advanced options tab
    -- -----------------------------------------------------------------
    padv <- panel nb []
    -- XMG tools
    viewCmdTxt <- entry padv
        [ tooltip := "Command used for XMG tree viewing"
        , text := getListFlagP ViewCmdFlg config ]
    let layXMG = fakeBoxed "XMG tools"
            [ row 3 [ label "XMG view command"
                    , marginRight $ hfill $ widget viewCmdTxt ] ]
    -- morphology
    morphFileLabel    <- staticText padv [ text := getListFlagP MorphInfoFlg config ]
    morphFileBrowseBt <- button padv [ text := browseTxt ]
    morphCmdTxt    <- entry padv
       [ tooltip := "Commmand used for morphological generation"
       , text    := getListFlagP MorphCmdFlg config ]
    let layMorph = fakeBoxed "Morphology"
            [ row 3 [ label "morph info:"
                    , expand $ hfill $ widget morphFileLabel
                    , widget morphFileBrowseBt ]
            , row 3 [ label "morph command"
                    , (marginRight.hfill) $ widget morphCmdTxt ] ]
    -- put the whole darn thing together
    let layAdvanced = hfloatLeft $ container padv
                    $ column 10 [ layXMG, layMorph ]
    -- -----------------------------------------------------------------
    -- browse button action
    --
    -- When the user clicks on a Browse button, an open file dialogue
    -- should pop up.  It gets its value from the file label on its left
    -- (passed in as an argument), and updates said label when the user has
    -- made a selection.
    -- -----------------------------------------------------------------
    -- helper functions
    curDir <- getCurrentDirectory
    let onBrowse theLabel = do
            rawFilename <- get theLabel text
            let filename = makeRelative curDir rawFilename
                filetypes = [("Any file",["*","*.*"])]
            fsel <- fileOpenDialog f False True
                      "Choose your file..." filetypes "" filename
            case fsel of
              -- if the user does not select any file there are no changes
              Nothing   -> return ()
              Just file -> set theLabel [ text := makeRelative curDir file ]
    -- end onBrowse
    -- activate those "Browse" buttons
    let setBrowse w l = set w [ on command := onBrowse l ]
    setBrowse macrosBrowseBt macrosFileLabel
    setBrowse lexiconBrowseBt lexiconFileLabel
    setBrowse tsBrowseBt tsFileLabel
    setBrowse morphFileBrowseBt morphFileLabel
    -- -----------------------------------------------------------------
    -- config GUI layout
    -- -----------------------------------------------------------------
    let parseRF  = parseFlagWithParsec "root features" geniFeats
        -- TODO: this is horrible! parseFlagWithParsec should be replaced with
        -- something safer
        onLoad
         = do macrosVal <- get macrosFileLabel text
              lexconVal <- get lexiconFileLabel text
              tsVal     <- get tsFileLabel text
              --
              detectPolsVal <- get detectPolsTxt text
              rootCatVal  <- get rootFeatTxt  text
              --
              viewVal   <- get viewCmdTxt text
              --
              morphCmdVal  <- get morphCmdTxt text
              morphInfoVal <- get morphFileLabel text
              --
              let maybeSet fl fn x =
                     if null x then deleteFlagP fl else setFlagP fl (fn x)
                  maybeSetStr fl = maybeSet fl id
              let setConfig = id
                    . maybeSetStr MacrosFlg    macrosVal
                    . maybeSetStr LexiconFlg   lexconVal
                    . maybeSetStr TestSuiteFlg tsVal
                    . maybeSetStr TestInstructionsFlg [(tsVal,Nothing)]
                    . setFlagP DetectPolaritiesFlg (readPolarityAttrs detectPolsVal)
                    . maybeSet RootFeatureFlg parseRF rootCatVal
                    . maybeSetStr ViewCmdFlg   viewVal
                    . maybeSetStr MorphCmdFlg  morphCmdVal
                    . maybeSetStr MorphInfoFlg morphInfoVal
              modifyIORef pstRef (modifyParams setConfig)
              loadFn
    -- end onLoad
      -- the button bar
    cancelBt <- button p [ text := "Cancel", on command := close f ]
    loadBt   <- button p [ text := "Load", on command := do { onLoad; close f } ]
    --
    set f [ layout := dynamic $ fill $ container p $ column 0
              [ fill $ tabs nb [ tab "Basic" layBasic
                               , tab "Advanced" layAdvanced ]
              , hfill $ row 1 [ hfloatLeft  (widget cancelBt)
                              , hfloatRight (widget loadBt) ]
              ]
          ]

-- --------------------------------------------------------------------
-- Generation
-- --------------------------------------------------------------------

-- | 'doGenerate' parses the target semantics, then calls the generator and
--   displays the result in a results gui (below).
doGenerate :: Textual tb => Window a -> ProgStateRef
                         -> tb -- ^ sem
                         -> tb -- ^ polarities to detect
                         -> tb -- ^ root feature
                         -> Bool -> Bool -> IO ()
doGenerate f pstRef sembox detectPolsTxt rootFeatTxt useDebugger pauseOnLex = do
    let parseRF  = parseFlagWithParsec "root features" geniFeats
    rootCatVal    <- get rootFeatTxt text
    detectPolsVal <- get detectPolsTxt text
    --
    let maybeSet fl fn x =
           if null x then deleteFlagP fl else setFlagP fl (fn x)
    let setConfig = id
                  . maybeSet RootFeatureFlg parseRF rootCatVal
                  . setFlagP DetectPolaritiesFlg (readPolarityAttrs detectPolsVal)
    modifyIORef pstRef (modifyParams setConfig)
    minput <- do
        set sembox [ text :~ trim ]
        loadEverything   pstRef
        parseSemInput <$> get sembox text
    case minput of
      Left e -> errorDialog f "Please give me better input" (show e)
      Right semInput -> do
          let doDebugger bg = debugGui   bg pstRef semInput pauseOnLex
              doResults  bg = resultsGui bg pstRef semInput
          catch
              (withBuilderGui $ if useDebugger then doDebugger else doResults)
              (handler "Error during realisation" prettyException)
  where
    handler title fn err = errorDialog f title (fn err)
    withBuilderGui a = do
      config <- pa <$> readIORef pstRef
      case builderType config of
        SimpleBuilder         -> a simpleGui2p
        SimpleOnePhaseBuilder -> a simpleGui1p

resultsGui :: BG.BuilderGui -> ProgStateRef -> SemInput -> IO ()
resultsGui builderGui pstRef semInput = do
    -- results window
    f <- frame [ text := "Results"
               , fullRepaintOnResize := False
               , layout := stretch $ label "Generating..."
               , clientSize := sz 300 300
               ]
    p    <- panel f []
    nb   <- notebook p []
    pst  <- readIORef pstRef
    -- input tab
    inputTab <- inputInfoGui nb (pa pst) semInput
    -- realisations tab
    (results,_,summTab,resTab) <- BG.resultsPnl builderGui pstRef semInput nb
    -- ranking tab
    mRankTab <- if hasFlagP RankingConstraintsFlg (pa pst)
                   then Just <$> messageGui nb (purty pst results)
                   else return Nothing
    -- tabs
    let myTabs = catMaybes
           [ Just (tab "summary"       summTab)
           , Just (tab "realisations"  resTab)
           , tab "ranking" <$> mRankTab
           , Just (tab "input"         inputTab)
           ]
    -- pack it all together
    set f [ layout := container p $ column 0 [ tabs nb myTabs ]
          , clientSize := bigSize ]
    repaint f
    return ()
  where
    purty pst res = unlines $ map (prettyResult pst) [ x | GSuccess x <- res ]


-- --------------------------------------------------------------------
-- Debugging
-- --------------------------------------------------------------------

-- | Information about the config/input in this session
inputInfoGui :: Window a -- ^ parent window
             -> Params
             -> SemInput
             -> IO Layout
inputInfoGui f config semInput = messageGui f . unlines $
    [ geniShow semInput
    , ""
    , "Options"
    , "-------"
    , "Root feature: " ++ maybe "" prettyStr (getFlagP RootFeatureFlg config)
    , ""
    , "Optimisations"
    , "-------------"
    ] ++ map optStatus optBios ++ polStuff

 where
  optStatus od = odShortTxt od ++ ": " ++
                 if enabld od then "Yes" else "No"
  enabld od = case odType od of
                Opti  -> configged od
                Pessi -> not (configged od)
  configged od = hasOpt  (odOpt od) config
  dps = maybe "" showPolarityAttrs (getFlagP DetectPolaritiesFlg config)
  eps = maybe "" showLitePm $ getFlagP ExtraPolaritiesFlg config
  polStuff = if enabld polarisedBio
              then [ ""
                   , "Detect polarities: " ++ dps
                   , "Extra polarities:  " ++ eps
                   ]
              else []

-- | We provide here a universal debugging interface, which makes use of some
--   parameterisable bits as defined in the BuilderGui module.
debugGui :: BG.BuilderGui -> ProgStateRef -> SemInput -> Bool -> IO ()
debugGui builderGui pstRef semInput pauseOnLex = do
    config <- pa <$> readIORef pstRef
    let btype = show (builderType config)
    --
    f <- frame [ text := "GenI Debugger - " ++ btype ++ " edition"
               , fullRepaintOnResize := False
               , clientSize := sz 300 300
               ]
    p    <- panel f []
    nb   <- notebook p []
    -- generation step 1
    (initStuff, initWarns) <- initGeni pstRef semInput
    let (cand,_)   = unzip $ B.inCands initStuff
    -- continuation for candidate selection tab
    let step2 newCands = do
           -- generation step 2.A (run polarity stuff)
           let newInitStuff = initStuff { B.inCands = map noBv newCands }
               (input2, autstuff) = B.preInit newInitStuff config
           -- automata tab
           mAutPnl <- if hasOpt Polarised config
                         then Just <$> myPolarityGui nb autstuff
                         else return Nothing
           -- generation step 2.B (start the generator for each path)
           debugPnl <- BG.debuggerPnl builderGui nb config input2 btype
           let mAutTab  = tab "automata" <$> mAutPnl
               debugTab = tab "tree assembly" debugPnl
               genTabs  = catMaybes [ mAutTab, Just debugTab ]
           --
           set f [ layout := container p $ tabs nb genTabs
                 , clientSize := bigSize
                 ]
           return ()
    -- inputs tab
    inpPnl <- inputInfoGui nb config semInput
    -- lexical selection tab
    pst <- readIORef pstRef
    (canPnl,_,_) <- pauseOnLexGui (pa pst) nb cand initWarns $
                       if pauseOnLex then Just step2 else Nothing
    -- basic tabs
    let basicTabs = [ tab "input"             inpPnl
                    , tab "lexical selection" canPnl ]
    --
    set f [ layout := container p $ tabs nb basicTabs
          , clientSize := bigSize ]
    -- display all tabs if we are not told to pause on lex selection
    unless pauseOnLex (step2 cand)
  where
    myPolarityGui nb autstuff =
       fst3 <$> polarityGui nb (prIntermediate autstuff) (prFinal autstuff)
    noBv x = (x, -1) -- all true?

-- ----------------------------------------------------------------------
-- odds and ends
-- ----------------------------------------------------------------------

bigSize :: Size2D Int
bigSize = sz 700 600

modifyParams :: (Params -> Params) -> ProgState -> ProgState
modifyParams f pst = pst { pa = f (pa pst) }

-- vim: set sw=4:
