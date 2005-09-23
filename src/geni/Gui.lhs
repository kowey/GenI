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

TODO
\begin{enumerate}
\item Tool tips - for the optimisations at least
\end{enumerate}

\begin{code}
module Gui(guiGenerate) where
\end{code}

\ignore{
\begin{code}
import Graphics.UI.WX

import qualified Control.Monad as Monad 
import Data.Array
import qualified Data.Map as Map
import Data.IORef
import Data.List (nub, delete, (\\))
import System.Directory 
import System.Process (runProcess)

import Graphviz 
import Treeprint(graphvizShowTagElem)

import Tags (tagLeaves)
import Morphology (sansMorph)
import Geni (ProgState(..), GeniInput(..), GeniResults(..), ProgStateRef,
             doGeneration, runGeni, runMorph,
             combine, loadGrammar, loadLexicon, 
             loadTargetSemStr)
import General (trim, fst3, snd3, slash, bugInGeni)
import Bfuncs (showPred, showSem, showPairs, Sem)
import Tags (idname,mapBySem,emptyTE,tsemantics,tpolarities,thighlight, 
             TagElem, derivation)

import Configuration(Params, Switch(..), 
                     macrosFile, lexiconFile, tsFile, viewCmd,
                     optimisations, 
                     usetrash,
                     autopol, polarised, polsig, chartsharing, 
                     semfiltered, extrapol, footconstr, ignoreSemantics)
import GeniParsers 

import Mstate (Gstats, Mstate, initGstats, initMState, runState, 
               generate, generateStep,  
               initrep, auxrep, genrep, trashrep,
               genstats, szchart, numcompar, geniter)
import Polarity
-- import Debug.Trace
\end{code}
}

\section{Main Gui}

\begin{code}
guiGenerate :: ProgStateRef -> IO() 
guiGenerate pstRef = do
  pst <- readIORef pstRef
  let gramConfig = pa pst
      -- errHandler title err = errorDialog f title (show err)
  loadGrammar pstRef -- `catch` (errHandler "Whoops!")
  start (mainGui pstRef)
\end{code}

This is the first screen that the user sees in the GenI interface.
We start things off by creating a frame and some menus.

\begin{code}
mainGui :: ProgStateRef -> IO ()
mainGui pstRef 
  = do --
       pst <- readIORef pstRef
       -- Top Window
       f <- frame [text := "Geni Project", clientSize := sz 600 350]
       -- create statusbar field
       status <- statusField   [text := "Welcome to GenI"]
       -- create the file menu
       fileMen   <- menuPane [text := "&File"]
       -- loadMenIt <- menuItem fileMen [text := "&Open index file"]
       quitMenIt <- menuQuit fileMen [text := "&Quit"]
       set quitMenIt [on command := close f]
       -- create the tools menu
       toolsMen      <- menuPane [text := "&Tools"]
       gbrowserMenIt <- menuItem toolsMen [ text := "&Inspect grammar" 
                                          , help := "Displays the trees in the grammar" ]
       -- create the help menu
       helpMen   <- menuPane [text := "&Help"]
       aboutMeIt <- menuAbout helpMen [help := "About"]
       -- Tie the menu to this window
       set f [ statusBar := [status] 
             , menuBar := [fileMen, toolsMen, helpMen]
             -- put the menu event handler for an about box on the frame.
             , on (menu aboutMeIt) := infoDialog f "About GenI" "The GenI generator" 
             -- event handler for the tree browser
             , on (menu gbrowserMenIt) := treeBrowserGui pstRef  
             ]
\end{code}

We add some buttons for loading files and running the generator.

\begin{code}
       let config     = pa pst 
           suite      = tsuite pst
           hasSem     = not (null suite)
           ignoreSem  = ignoreSemantics config
       -- Target Semantics
       tsTextBox <- textCtrl f [ wrap := WrapWord
                               , clientSize := sz 400 80
                               , enabled := hasSem 
                               , text := if ignoreSem
                                         then "% --ignoresemantics set" else "" ]
       testCaseChoice <- choice f [ selection := 0 
                                  , enabled := hasSem ]
       readTestSuite pstRef tsTextBox testCaseChoice 
       -- Box and Frame for files loaded 
       macrosFileLabel  <- staticText f [ text := macrosFile config  ]
       lexiconFileLabel <- staticText f [ text := lexiconFile config ]
       tsFileLabel      <- staticText f [ text := tsFile config ]
       -- Generate and Debug 
       debugBt <- button f [ text := "  Debug  "
                           , on command := doGenerate f pstRef tsTextBox True ]
       genBt  <- button f [text := "  Generate  ",
                 on command := doGenerate f pstRef tsTextBox False ]
\end{code}

Let's not forget the optimisations...

\begin{code}
       polChk <- checkBox f [ text := "Polarities"
                            , checked := polarised config ]
       autopolChk <- checkBox f [ text := "Pol detection"
                                , checked := autopol config ]
       polsigChk <- checkBox f [ text := "Pol signatures"
                               , checked := polsig config ]
       chartsharingChk <- checkBox f [ text := "Chart sharing"
                                     , checked := chartsharing config ]
       semfilterChk <- checkBox f [ text := "Semantic filters"
                                  , checked := semfiltered config ]
       orderedadjChk <- checkBox f [ text := "Ordered adjunction"
                                  , checked := True
                                  , enabled := False ]
       footconstrChk <- checkBox f [ text := "Foot constraint"
                                   , checked := footconstr config ]
       extrapolText <- staticText f [ text := showLitePm $ extrapol config ]
       -- commands for the checkboxes
       let togglePolStuff = do c <- get polChk checked
                               set autopolChk      [ enabled := c ]
                               set polsigChk       [ enabled := c ]
                               set chartsharingChk [ enabled := c ]
                               set extrapolText    [ enabled := c ] 
       set polChk          [on command := do togglePolStuff
                                             toggleChk pstRef polChk PolarisedTok ] 
       set autopolChk      [on command := toggleChk pstRef autopolChk AutoPolTok ] 
       set polsigChk       [on command := toggleChk pstRef polsigChk PolSigTok] 
       -- set predictingChk   [on command := toggleChk pstRef predictingChk Predicting] 
       set chartsharingChk [on command := toggleChk pstRef chartsharingChk ChartSharingTok]
       set semfilterChk    [on command := toggleChk pstRef semfilterChk SemFilteredTok] 
       set footconstrChk   [on command := toggleChk pstRef footconstrChk FootConstraintTok] 
\end{code}

Pack it all together, perform the layout operation.

\begin{code}
       -- set any last minute handlers, run any last minute functions
       -- let guiParts = (macrosFileLabel, lexiconFileLabel, tsFileLabel, tsTextBox, testCaseChoice)
       -- set loadMenIt [ on command := loadMenuCmd pstRef f guiParts ]  
       -- readGrammar pstRef guiParts 
       togglePolStuff
       --
       let gramsemBox = boxed "Files last loaded" $ -- can't used boxed with wxwidgets 2.6 -- bug?
                   hfill $ column 1 
                     [ row 1 [ label "trees:", widget macrosFileLabel  ]
                     , row 1 [ label "lexicon:", widget lexiconFileLabel ] 
                     , row 1 [ label "test suite:", widget tsFileLabel ] 
                     ]
           optimBox =  --boxed "Optimisations " $ -- can't used boxed with wxwidgets 2.6 -- bug?
                    column 5 [ label "Optimisations" 
                             , dynamic $ widget polChk 
                             , row 5 [ label "  ", column 5 
                                     [ dynamic $ row 5 [ label "Extra: ", widget extrapolText ]
                                     , dynamic $ widget autopolChk 
                                     , dynamic $ widget polsigChk
                                     , dynamic $ widget chartsharingChk ] ]
                             , dynamic $ widget semfilterChk 
                             , dynamic $ widget orderedadjChk 
                             , dynamic $ widget footconstrChk 
                             --, dynamic $ widget predictingChk
                             ]
       set f [layout := column 5 [ gramsemBox
                   , row 5 [ fill $ -- boxed "Input Semantics" $ 
                             hfill $ column 5 
                               [ hfill $ row 1 [ label "Test case", hfill $ widget testCaseChoice ]
                               , fill  $ widget tsTextBox ]
                           , vfill optimBox ]
                    -- ----------------------------- Generate and quit 
                   , hfloatRight $ row 5 [ widget debugBt, widget genBt 
                                       -- , widget quitBt 
                                       ]
                   ]]
\end{code}

Don't forget all the helper functions!

\subsection{Configuration}

Toggles for optimisatons controlled by a check box.  They enable or 
disable the said optmisation.

\begin{code}
toggleChk :: (Checkable a) => ProgStateRef -> a -> Switch -> IO ()
toggleChk pstRef chk tok = do
  isChecked <- get chk checked
  let fn config = config { optimisations = nub newopt }
                  where opt = optimisations config 
                        newopt = if isChecked then tok:opt else delete tok opt
  modifyIORef pstRef (\x -> x{pa = fn (pa x)})
  return ()
\end{code}

\paragraph{loadMenuCmd} is meant to be the handler executed when you
select the "Open index file" menu item.  It loads the grammar and 
updates all the relevant GUI widgets.

\begin{code}
{-
loadMenuCmd :: (Textual b, Able c, Selecting c, Selection c, Items c String) 
               => ProgStateRef -> Window a -> (StaticText d, b, c, c) -> IO ()
loadMenuCmd pstRef f guiParts = 
  do pst <- readIORef pstRef
     let filename  = grammarFile (pa pst)
     filetypes = [("Any file",["*","*.*"])]
     fsel <- fileOpenDialog f False True "Choose your file..." filetypes "" filename
     case fsel of
       -- if the user does not select any file there are no changes
       Nothing       -> return () 
       Just file     -> loadMenuHelper pstRef f (trim file) guiParts

loadMenuHelper pstRef f filename guiParts = 
  do -- write the new values
     let newPa p = p { grammarFile = filename }
     modifyIORef pstRef (\x -> x{pa = newPa (pa x)})
     -- load in the files
     let (gText, tsBox, tsChoice, lexChoice) = guiParts
     loadGrammar pstRef 
     readGrammar pstRef gText lexChoice
     readTestSuite pstRef tsBox tsChoice
  `catch` (errHandler "Error loading the grammar or test suite: ")
  where errHandler title err = errorDialog f title (show err)
-}
\end{code}

% --------------------------------------------------------------------
\section{Loading files}
% --------------------------------------------------------------------

\paragraph{readGrammar} is used to update the graphical interface after
calling \fnref{loadGrammar}. This used when you first start the 
graphical interface or when you load a grammar.

FIXME: do we still need this bit of code? 
\begin{code}
readGrammar :: (Textual a) => ProgStateRef -> a -> IO ()
readGrammar pstRef gText = return ()
\end{code}

\paragraph{readTestSuite} is used to update the graphical interface after
calling \fnref{loadTestSuite}.  This used when you first start the 
graphical interface or when you load a new test suite.

\begin{code}
readTestSuite :: (Textual a, Selecting b, Selection b, Items b String) 
              => ProgStateRef -> a -> b -> IO ()
readTestSuite pstRef tsBox tsChoice = 
  do pst <- readIORef pstRef
     let suite = tsuite pst
         cases = tcases pst
         suiteCases = map fst3 suite 
     -- we number the cases for easy identification, putting 
     -- a star to highlight those which are in TestCases
     let numfn n t = (if t `elem` cases then "* " else "")
                      ++ (show n) ++ ". " ++ t
         tcaseLabels = zipWith numfn [1..] suiteCases 
     -- we select the first case in cases_, if available
     let fstInCases _ [] = 0 
         fstInCases n (x:xs) = 
           if (x `elem` cases) then n else fstInCases (n+1) xs
         caseSel = if null cases then 0 
                   else fstInCases 0 suiteCases
     ----------------------------------------------------
     -- handler for selecting a test case
     ----------------------------------------------------
     let displaySemInput (s,r) = 
              "semantics: " ++ showSem s 
           ++ (if null r then "" 
               else "\nrestrictors:" ++ showPairs r)
     let onTestCaseChoice = do
         csel <- get tsChoice selection
         if (boundsCheck csel suite)
           then do let s = snd3 (suite !! csel)
                   set tsBox [ text :~ (\_ -> displaySemInput s) ]
           else fail ("Gui: test case selector bounds check error: " ++
                      show csel ++ " of " ++ show suite ++ "\n" ++
                      bugInGeni)
     ----------------------------------------------------
     set tsChoice [ items := tcaseLabels 
                  , selection := caseSel
                  , on select := onTestCaseChoice ]
     when (not $ null suite) onTestCaseChoice -- run this once
\end{code}
   
% --------------------------------------------------------------------
\section{Results}
\label{sec:results_gui}
% --------------------------------------------------------------------

\paragraph{doGenerate} parses the target semantics, then calls the
generator and displays the result in a results gui (below).

\begin{code}
doGenerate :: Textual b => Window a -> ProgStateRef -> b -> Bool -> IO ()
doGenerate f pstRef sembox debugger = do 
  let handler title err = errorDialog f title (show err)
      handler1 err = handler "Error (probably the target semantics): " err 
      -- handler2 err = do handler "Error during generation:" err
                        --return GR { grCand = [], grAuts = [], grCombos = [], 
                        --grFinalAut = emptyaut, grDerived  = [], grStats    = ""}
  sem <- get sembox text
  let dodebug = do loadTargetSemStr pstRef sem
                   runGeni pstRef debugGui 
                   return ()
      dogen = do loadTargetSemStr pstRef sem
                 res <- runGeni pstRef doGeneration 
                 resultsGui pstRef res 
  -- FIXME: it would be nice to distinguish between generation and ts
  -- parsing errors
  (if debugger then dodebug else dogen) `catch` handler1
\end{code}

\paragraph{resultsGui} displays generation result in a window.  The window
consists of various tabs for intermediary results in lexical
selection, derived trees, derivation trees and generation statistics.

\begin{code}
resultsGui :: ProgStateRef -> GeniResults -> IO () 
resultsGui pstRef res = do
  -- results window
  f <- frame [ text := "Results" 
             , fullRepaintOnResize := False 
             , layout := stretch $ label "Generating..."
             , clientSize := sz 300 300 
             ] 
  p    <- panel f []
  nb   <- notebook p []
  -- realisations tab
  resTab <- realisationsGui pstRef nb (grDerived res)
  -- statistics tab
  statTab <- statsGui nb (show res)
  -- pack it all together 
  set f [ layout := container p $ column 0 [ tabs nb 
        -- we put the realisations tab last because of what
        -- seems to be buggy behaviour wrt to wxhaskell 
        -- or wxWidgets 2.4 and the splitter
               [ tab "summary"       statTab
               , tab "realisations"  resTab ] ]
        , clientSize := sz 700 600 
        ] 
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

\subsection{Lexically selected items}

We have a browser for the lexically selected items.  We group the lexically
selected items by the semantics they subsume, inserting along the way some
fake trees and labels for the semantics.

\begin{code}
candidateGui :: ProgState -> (Window a) -> [TagElem] -> Sem -> IO Layout
candidateGui pst f xs missed = do
  p  <- panel f []      
  tb <- tagBrowserGui pst p xs "lexically selected item" "candidates"
  let warning = if null missed 
                then ""
                else "WARNING: no lexical selection for " ++ showSem missed
      items = if null missed then [ fill tb ] else [ hfill (label warning) , fill tb ]
      lay   = fill $ container p $ column 5 items
  return lay
\end{code}
      
\subsection{Polarity Automata}

A browser to see the automata constructed during the polarity optimisation
step.

\begin{code}
polarityGui :: (Window a) -> [(String,PolAut,PolAut)] -> PolAut -> IO Layout
polarityGui   f xs final = do
  let numsts a = " : " ++ (show n) ++ " states" 
                 where n = foldr (+) 0 $ map length $ states a 
      aut2  (_ , a1, a2) = [ a1, a2 ]
      autLabel (fv,a1,_) = [ fv ++ numsts a1, fv ++ " pruned" ]
      autlist = map toGvPolAut $ (concatMap aut2 xs) ++ [ final ] 
      labels  = (concatMap autLabel xs) ++ [ "final" ++ numsts final ]
      --
  gvRef   <- newGvRef False labels "automata"
  setGvDrawables gvRef autlist
  (lay,_) <- graphvizGui f "polarity" gvRef 
  return lay
\end{code}
      
\subsection{Derived Trees}

Browser for derived/derivation trees, except if there are no results, we show a
message box

\begin{code}
realisationsGui :: ProgStateRef -> (Window a) -> [TagElem] -> IO Layout
realisationsGui _   f [] = messageGui f "No results found"
realisationsGui pstRef f resultsRaw = 
  do let results = map (\t -> t {thighlight = []}) resultsRaw
     -- morphology
     let uninflected = map tagLeaves resultsRaw 
     sentences <- runMorph pstRef uninflected
     --
     let itNlabl   = zip results sentences
         tip       = "result"
     --
     pst     <- readIORef pstRef
     (lay,_) <- tagViewerGui pst f tip "derived" itNlabl
     return lay
\end{code}

\subsection{TAG viewer and browser}

A TAG browser is a TAG viewer (see below) that groups trees by 
their semantics.

\begin{code}
tagBrowserGui :: ProgState -> (Window a) -> [TagElem] -> String -> String -> IO Layout
tagBrowserGui pst f xs tip cachedir = do 
  let semmap   = mapBySem xs
      sem      = Map.keys semmap
      --
      lookupTr k = Map.findWithDefault [] k semmap
      treesfor k = emptyTE : (lookupTr k)
      labsfor  k = ("___" ++ showPred k ++ "___") : (map fn $ lookupTr k)
                   where fn t = idname t 
      --
      trees    = concatMap treesfor sem
      labels   = concatMap labsfor  sem
      itNlabl  = zip trees labels
  (lay,_) <- tagViewerGui pst f tip cachedir itNlabl
  return lay
\end{code}
      
A TAG viewer is a graphvizGui that lets the user toggle the display
of TAG feature structures.

\begin{code}
tagViewerGui :: ProgState -> (Window a) -> String -> String -> [(TagElem,String)] 
               -> GvIO TagElem
tagViewerGui pst f tip cachedir itNlab = do
  p <- panel f []      
  let (tagelems,labels) = unzip itNlab
  gvRef <- newGvRef False labels tip
  setGvDrawables gvRef tagelems 
  (lay,updaterFn) <- graphvizGui p cachedir gvRef 
  -- widgets
  detailsChk <- checkBox p [ text := "Show features"
                           , checked := False ]
  displayTraceBut <- button p [ text := "Display trace for" ]
  displayTraceCom <- choice p [ tooltip := "derivation tree" ]
  -- handlers
  let onDisplayTrace = do gvSt <- readIORef gvRef
                          s <- get displayTraceCom selection
                          let tsel = gvsel gvSt
                          Monad.when (boundsCheck tsel tagelems) $ do
                            let tree = tagelems !! (gvsel gvSt)
                                derv = extractDerivation tree
                            if (boundsCheck s derv)
                              then runViewTag pst (derv !! s)
                              else fail $ "Gui: bounds check in onDisplayTrace\n" ++
                                          bugInGeni
  let onDetailsChk c = do isDetailed <- get c checked 
                          setGvParams gvRef isDetailed 
                          updaterFn 
  let selHandler gvSt = do
      let tsel = gvsel gvSt
      Monad.when (boundsCheck tsel tagelems) $ do
        let selected = tagelems !! tsel 
            subtrees = extractDerivation selected
        set displayTraceCom [ items :~ (\_ -> subtrees)
                            , selection :~ (\_ -> 0) ]
  --
  Monad.when (not $ null tagelems) $ do 
    setGvHandler gvRef (Just selHandler)
    set detailsChk [ on command := onDetailsChk detailsChk ]
    set displayTraceBut [ on command := onDisplayTrace ]
  -- pack it all in      
  let cmdBar = hfill $ row 5 
                [ dynamic $ widget detailsChk
                , dynamic $ widget displayTraceBut
                , dynamic $ widget displayTraceCom 
                ]
      lay2   = fill $ container p $ column 5 [ lay, cmdBar ] 
  return (lay2,updaterFn)
\end{code}

% --------------------------------------------------------------------
\section{Tree browser}
\label{sec:treebrowser_gui}
% --------------------------------------------------------------------

This is a very simple semantically-separated browser for all the
trees in the grammar.  Note that we can't just reuse candidateGui's
code because we label and sort the trees differently.  Here we 
ignore the arguments in tree semantics, and we display the tree
polarities in its label.

\begin{code}
treeBrowserGui :: ProgStateRef -> IO () 
treeBrowserGui pstRef = do
  pst <- readIORef pstRef
  -- ALL THE TREES in the grammar... muahahaha!
  let semmap = combine (gr pst) (le pst)
  -- browser window
  f <- frame [ text := "Tree Browser" 
             , fullRepaintOnResize := False 
             ] 
  -- the heavy GUI artillery
  let sem      = Map.keys semmap
      --
      lookupTr k = Map.findWithDefault [] k semmap
      treesfor k = emptyTE : (lookupTr k)
      labsfor  k = ("___" ++ k ++ "___") : (map fn $ lookupTr k)
                   where fn    t = idname t ++ polfn (tpolarities t)
                         polfn p = if Map.null p 
                                   then "" 
                                   else " (" ++ showLitePm p ++ ")"
      --
      trees    = concatMap treesfor sem
      itNlabl  = zip trees (concatMap labsfor sem)
  (browser,_) <- tagViewerGui pst f "tree browser" "grambrowser" itNlabl
  -- the button panel
  let count = length trees - length sem
  quitBt <- button f [ text := "Close", on command := close f ]
  -- pack it all together 
  set f [ layout := column 5 [ browser, 
                       row 5 [ label ("number of trees: " ++ show count)
                             , hfloatRight $ widget quitBt ] ]
        , clientSize := sz 700 600 ]
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
debugGui :: ProgState -> GeniInput -> IO ([TagElem], Gstats)
debugGui pst input = do
  let tsem = giSem input
  --
  f <- frame [ text := "Geni Debugger" 
             , fullRepaintOnResize := False 
             , clientSize := sz 300 300 
             ] 
  p    <- panel f []
  nb   <- notebook p []
  -- 
  {-
  -- create an information tab (FIXME: this is mostly a hack to hide
  -- a GUI bug with the first debugger tab under Linux) 
  pinfo <- panel nb []
  let infoText = "Input Semantics: " ++ showSem tsem
      infoLay  = fill $ container pinfo $ margin 1 $ label infoText
      infoTab  = tab "info" infoLay
  -}
  -- candidate selection tab
  let cand    = giCands input
      candsem = (nub $ concatMap tsemantics cand)
      missed  = tsem \\ candsem
  canTab <- candidateGui pst nb cand missed
  -- automata tab
  let config           = pa pst
      (auts, finalaut) = giAuts input 
  autTab <- if polarised config 
            then polarityGui nb auts finalaut
            else messageGui nb "polarities disabled"
  -- basic tabs 
  let basicTabs = tab "lexical selection" canTab :
                  (if polarised config then [tab "automata" autTab] else [])
  -- start the generator for each path
  let combos = giTrees input
      tabLabels = map (\x -> "session " ++ show x) [1..] 
      createTab (cd,xs) = debuggerTab nb config tsem cd xs
  debugTabs <- mapM createTab $ zip tabLabels combos
  let genTabs = map fn $ zip tabLabels debugTabs
                where fn (l,t) = tab l t
  --
  set f [ layout := container p $ column 0 
          [ tabs nb (basicTabs ++ genTabs) ]
        , clientSize := sz 700 600      
        ]
  return ([], initGstats)
\end{code}

The generation could conceivably be broken into multiple generation
tasks, so we create a separate tab for each task.

\begin{code}
debuggerTab :: (Window a) -> Params -> Sem -> String -> [TagElem] -> IO Layout 
debuggerTab f config tsem cachedir cands = do
  let initRes = []
      initSt  = initMState cands [] tsem (config {usetrash=True})
  let (items,labels) = showGenState initRes initSt 
  -- widgets
  p <- panel f []      
  gvRef <- newGvRef False labels "debugger session" 
  setGvDrawables gvRef items 
  (lay,updaterFn) <- graphvizGui p cachedir gvRef 
  detailsChk <- checkBox p [ text := "Show features"
                           , checked := False ]
  restartBt   <- button p [text := "Start over"]
  nextBt   <- button p [text := "Leap by..."]
  leapVal  <- textEntry p [ text := "1", clientSize := sz 30 25 ]
  finishBt <- button p [text := "Continue"]
  statsTxt <- staticText p []
  -- commands
  let updateStatsTxt gs = set statsTxt [ text :~ (\_ -> txtStats gs) ]
      txtStats   gs =  "itr " ++ (show $ geniter gs) ++ " " 
                    ++ "chart sz: " ++ (show $ szchart gs) 
                    ++ "\ncomparisons: " ++ (show $ numcompar gs)
  let onDetailsChk = do isDetailed <- get detailsChk checked 
                        setGvParams gvRef isDetailed
                        updaterFn
  let genStep _ (r2,s2) = (r2 ++ r3,s3)
                          where (r3,s3) = runState generateStep s2
  let showNext r s = do leapTxt <- get leapVal text
                        let leapInt = read leapTxt
                            (r2,s2) = foldr genStep (r,s) [1..leapInt]
                        setGvDrawables2 gvRef (showGenState r2 s2)
                        setGvSel gvRef 1
                        updaterFn
                        updateStatsTxt (genstats s2)
                        set nextBt [ on command :~ (\_ -> showNext r2 s2) ]
  let showLast = do -- redo generation from scratch
                    let (r,s) = runState generate initSt 
                    setGvDrawables2 gvRef (showGenState r s)
                    updaterFn
                    updateStatsTxt (genstats s)
  let showReset = do let res = initRes 
                         st  = initSt 
                     set nextBt   [ on command  := showNext res st ]
                     updateStatsTxt initGstats
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
showGenState :: [TagElem] -> Mstate -> ([TagElem],[String])
showGenState res st = 
  let agenda    = initrep st
      auxiliary = auxrep st
      trash     = trashrep st
      chart     = genrep  st
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
      labelFn trs = map fn trs 
        where fn t = (sansMorph.tagLeaves) t ++ " (" ++ showPolPaths t ++ ")"
  in (trees,labels)
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
               -- updated
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

setGvDrawables gvref it =
  do let fn x = x { gvitems = array (0, length it) (zip [0..] it),
                    gvorders = GvoItems : (gvorders x) }
     modifyIORef gvref fn 

setGvDrawables2 gvref (it,lb) =
  do let fn x = x { gvlabels = lb }
     modifyIORef gvref fn 
     setGvDrawables gvref it

setGvHandler gvref h =
  do gvSt <- readIORef gvref
     modifyIORef gvref (\x -> x { gvhandler = h })
     case h of 
       Nothing -> return ()
       Just fn -> fn gvSt
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

Returns: a function for updating the GUI 
(args for the updater function are itNlab and the index you want to select or
 -1 to keep the same selection)

%\begin{code}
%graphvizGui :: (GraphvizShow d) => 
%  (Window a) -> String -> GraphvizRef d Bool -> GvIO d
%type GvIO d = IO (Layout, IO ())
%graphvizGui f cachedir gvRef = do
%  initGvSt <- readIORef gvRef
%  rchoice  <- singleListBox f 
%              [items := gvlabels initGvSt,
%               tooltip := gvtip initGvSt]
%  let lay = fill $ widget rchoice
%  return (lay, return () )  
%\end{code}

\begin{code}
graphvizGui :: (GraphvizShow d) => 
  (Window a) -> String -> GraphvizRef d Bool -> GvIO d
type GvIO d = IO (Layout, IO ())
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
  let showItem = createAndOpenImage cachedir p gvRef openFn
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
        updaterFn
        gvSt <- readIORef gvRef
        -- call the handler if there is one 
        case (gvhandler gvSt) of 
          Nothing -> return ()
          Just h  -> h gvSt
  ------------------------------------------------
  set rchoice [ on select := selectAndShow ]
  -- call the updater function for the first time
  -- setGvSel gvRef 1
  updaterFn 
  -- return a layout and the updater function 
  return (lay, updaterFn)
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
createAndOpenImage :: (GraphvizShow b) => 
  FilePath -> Window a -> GraphvizRef b Bool -> OpenImageFn -> IO ()
createAndOpenImage cachedir f gvref openFn = do 
  r <- createImage cachedir f gvref 
  case r of 
    Just graphic -> openFn graphic
    Nothing -> return ()
\end{code}

\paragraph{createImage}
Creates a graphical visualisation for anything which can be displayed
by graphviz. Arguments: a cache directory, a WxHaskell window, and index and an
array of trees.  Returns Just filename if the index is valid or Nothing
otherwise 

\begin{code}
createImage :: (GraphvizShow b) => 
  FilePath -> Window a -> GraphvizRef b Bool -> IO (Maybe FilePath) 
createImage cachedir f gvref = do
  gvSt <- readIORef gvref
  -- putStrLn $ "creating image via graphviz"
  let drawables = gvitems  gvSt
      sel       = gvsel    gvSt
      config    = gvparams gvSt
      te = (drawables ! sel)
      b  = bounds drawables 
      dotFile     = createDotPath cachedir (show sel)
      graphicFile = createImagePath cachedir (show sel)
      create = do toGraphviz config te dotFile graphicFile
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
  cmainExists <- doesDirectoryExist gv_CACHEDIR
  Monad.when (not cmainExists) $ createDirectory gv_CACHEDIR 
  -- 
  let cachedir = gv_CACHEDIR ++ "/" ++ cachesubdir  
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

A message panel for use by the Results gui panels \ref{sec:results_gui}.

\begin{code}
messageGui :: (Window a) -> String -> IO Layout 
messageGui f msg = do 
  p <- panel f []
  -- sw <- scrolledWindow p [scrollRate := sz 10 10 ]
  t  <- textCtrl p [ text := msg, enabled := False ]
  return (fill $ container p $ column 1 $ [ fill $ widget t ]) 
\end{code}

\begin{code}
gv_CACHEDIR :: String
gv_CACHEDIR = ".gvcache"

-- FIXME: should use OS-independant seperator
createImagePath :: String -> String -> String
createImagePath subdir name = 
  gv_CACHEDIR ++ "/" ++ subdir ++ "/" ++ name ++ ".png"

createDotPath :: String -> String -> String
createDotPath subdir name = 
  gv_CACHEDIR ++ "/" ++ subdir ++ "/" ++ name ++ ".dot"
\end{code}

\paragraph{boundsCheck} makes sure that index s is in the bounds of list l.
This is useful for the various blocks of code that manipulate wxhaskell
selections.  Surely there must be some more intelligent way to deal with
this.

\begin{code}
boundsCheck s l = s >= 0 && s < length l
\end{code}

\begin{code}
instance GraphvizShow TagElem where
  graphvizShow = graphvizShowTagElem
\end{code}

\subsection{XMG Metagrammar stuff}

CGM trees are produced by the XMG metagrammar system
(\url{http://sourcesup.cru.fr/xmg/}). To debug these grammars, it is
useful, given a TAG tree, to see what its metagrammar origins are.  We
provide here an interface to the handy visualisation tool ViewTAG that
just does this.

\paragraph{extractDerivation} retrieves the names of all the
CGM trees that went to building a TagElem, including the TagElem
itself.  NB: for a tree like ``love\_Tn0Vn1'', we extract just the
Tn0Vn1 bit.

\begin{code}
extractDerivation :: TagElem -> [String]
extractDerivation te = 
  let -- strips all gorn addressing stuff
      stripGorn n = if dot `elem` n then stripGorn stripped else n
        where stripped =  (tail $ dropWhile (/= dot) n)
              dot = '.'
      deriv  = map (stripGorn.snd3) $ snd $ derivation te
  in  nub (idname te : deriv)
\end{code}

\paragraph{runViewTag} runs Yannick Parmentier's ViewTAG module, which
displays trees produced by the XMG metagrammar system.  

\begin{code}
runViewTag :: ProgState -> String -> IO ()
runViewTag pst idname =  
  do -- figure out what grammar file to use
     let params  = pa pst
         gramfile = macrosFile params
     -- extract the relevant bits of the treename
     let extractCGMName n = tail $ dropWhile (/= '_') n 
         drName = extractCGMName idname 
     -- run the viewer 
     let cmd  = viewCmd params 
         args = [gramfile, drName]
     -- run the viewer
     runProcess cmd args Nothing Nothing Nothing Nothing Nothing
     return ()
\end{code}

