\chapter{Graphical User Interface} 

\begin{enumerate}
\item Tool tips - for the optimisations at least
\end{enumerate}

\begin{code}
module Gui(guiGenerate) where
\end{code}

\ignore{
\begin{code}
import Graphics.UI.WXCore 
import Graphics.UI.WX

import IOExts(readIORef, modifyIORef)
import Monad (when)
import Data.Array
import Data.Char (isSpace)
import Data.FiniteMap
import Data.List (nub, delete)
import System.Directory 

import Graphviz 
import Treeprint
import Geni (State(..), GeniResults(..), PState,
             runGeni, customGeni, combine,
             loadMacros, loadLexicon, loadTargetSemStr)
import Btypes (showSem, showPred, Sem)
import Tags (idname,mapBySem,emptyTE,tsemantics,tpolarities,
             showfeats,TagElem)

import Configuration(Params, GrammarType(..), 
                     macrosFile, lexiconFile, grammarType,
                     tsFile, optimisations, 
                     polarised, polsig, chartsharing, 
                     semfiltered, orderedadj, extrapol, footconstr)
import ParserLib 

import Mstate (Gstats, Mstate, initGstats, initMState, runState, 
               generate, generateStep, initrep, auxrep, genrep,
               genRepToList,
               genstats, szchart, numcompar, geniter)
import Polarity
\end{code}
}

\section{Main Gui}

\begin{code}
guiGenerate :: PState -> IO() 
guiGenerate pst = do
  loadMacros pst
  loadLexicon pst
  start (mainGui pst)
\end{code}

\begin{code}
mainGui :: PState -> IO ()
mainGui pst = do 
       mst <- readIORef pst
       -- Top Window
       f <- frame [text := "Geni Project", clientSize := sz 600 350]
       -- create statusbar field
       status <- statusField   [text := "Welcome to GenI"]
\end{code}

We set up the menu and the status bars.

\begin{code}
       -- create the file menu
       fileMen   <- menuPane [text := "&File"]
       quitMenIt <- menuQuit fileMen [text := "Quit"]
       set quitMenIt [on command := close f]
       -- create the tools menu
       toolsMen      <- menuPane [text := "&Tools"]
       gbrowserMenIt <- menuItem toolsMen [ text := "&Inspect grammar" 
                                          , help := "Displays the trees in the grammar" 
                                          ]

       -- create the help menu
       helpMen   <- menuPane [text := "&Help"]
       aboutMeIt <- menuAbout helpMen [help := "About"]
       -- Tie the menu to this window
       set f [ statusBar := [status] 
             , menuBar := [fileMen, toolsMen, helpMen]
             -- put the menu event handler for an about box on the frame.
             , on (menu aboutMeIt) := infoDialog f "About GenI" "The GenI generator" 
             -- event handler for the tree browser
             , on (menu gbrowserMenIt) := treeBrowserGui pst  
             ]
\end{code}

We add some buttons for loading files and running the generator.

\begin{code}
       let config = pa mst 
       -- Target Semantics
       tsText <- readFile (tsFile $ pa mst)
       tsTextBox <- textCtrl f WrapWord [ text := tsText, clientSize := sz 300 80 ]
       -- Box and Frame for files loaded 
       let gFilename = macrosFile  config 
           lFilename = lexiconFile config

       grammarFileLabel <- staticText f [ text := gFilename ]
       lexiconFileLabel <- staticText f [ text := lFilename ]
       tsFileLabel      <- staticText f [ text := (trim.tsFile) config ]
       let guiParts = (grammarFileLabel, lexiconFileLabel, tsFileLabel, 
                       tsTextBox)
       loadfileBt <- button f [ text := "Load files"
                              , on command := gramsemBrowser pst guiParts ] 
       --reloadSemBt <- button f [ text := "Reload semantics"
       --                        , on command := readTargetSem pst tsTextBox ] 
       -- Configuration
       polChk <- checkBox f [ text := "Polarities"
                            , checked := polarised config ]
--       predictingChk <- checkBox f [ text := "Predictors"
--                                   , checked := predicting config ]
       polsigChk <- checkBox f [ text := "Pol signatures"
                               , checked := polsig config ]
       chartsharingChk <- checkBox f [ text := "Chart sharing"
                                     , checked := chartsharing config ]
       semfilterChk <- checkBox f [ text := "Semantic filters"
                                  , checked := semfiltered config ]
       orderedadjChk <- checkBox f [ text := "Ordered adjunction"
                                  , checked := orderedadj config ]
       footconstrChk <- checkBox f [ text := "Foot constraint"
                                   , checked := footconstr config ]
       extrapolText <- staticText f [ text := showLitePm $ extrapol config ]
       -- commands for the checkboxes
       let togglePolStuff = do c <- get polChk checked
                               set polsigChk       [ enabled := c ]
                               set chartsharingChk [ enabled := c ]
                               set extrapolText    [ enabled := c ] 
       set polChk          [on command := do togglePolStuff
                                             toggleChk pst polChk Polarised ] 
       set polsigChk       [on command := toggleChk pst polsigChk PolSig] 
       -- set predictingChk   [on command := toggleChk pst predictingChk Predicting] 
       set chartsharingChk [on command := toggleChk pst chartsharingChk ChartSharing]
       set semfilterChk    [on command := toggleChk pst semfilterChk SemFiltered] 
       set orderedadjChk   [on command := toggleChk pst orderedadjChk OrderedAdj]
       set footconstrChk   [on command := toggleChk pst footconstrChk FootConstraint] 
       -- Generate and Debug 
       debugBt <- button f [ text := "  Debug  "
                           , on command := doGenerate f pst tsTextBox True ]
       genBt  <- button f [text := "  Generate  ",
                 on command := doGenerate f pst tsTextBox False ]
\end{code}
      
Pack it all together.

\begin{code}
       togglePolStuff
       --
       let gramsemBox = boxed "Files last loaded" $
                   row 5 [ column 5 [ row 5 [ label "trees: ", widget grammarFileLabel ]
                                    , row 5 [ label "lexicon: ", widget lexiconFileLabel ]
                                    , row 5 [ label "input sem: ", widget tsFileLabel ] ] 
                         , floatBottomRight $ column 5 [ hfloatRight $ widget loadfileBt ] 
                         ] 
           optimBox =  boxed "Optimisations " $
                    column 5 [ dynamic $ widget polChk 
                             , row 5 [ label "  ", column 5 
                                     [ dynamic $ row 5 [ label "Extra: ", widget extrapolText ]
                                     , dynamic $ widget polsigChk
                                     , dynamic $ widget chartsharingChk ] ]
                             , dynamic $ widget semfilterChk 
                             , dynamic $ widget orderedadjChk 
                             , dynamic $ widget footconstrChk 
                             --, dynamic $ widget predictingChk
                             ]
       set f [layout := column 5 [ gramsemBox
                   , row 5 [ fill $ boxed "Input Semantics" $ column 5 [ fill $ widget tsTextBox ]  
                           , vfill optimBox ]
                    -- ----------------------------- Generate and quit 
                   , hfloatRight $ row 5 [ widget debugBt, widget genBt 
                                       -- , widget quitBt 
                                       ]
                   ]]
\end{code}

\subsection{Configuration}

Toggles for optimisatons controlled by a check box.  They enable or 
disable the said optmisation.

\begin{code}
toggleChk :: (Checkable a) => PState -> a -> Token -> IO ()
toggleChk pst chk tok = do
  isChecked <- get chk checked
  let fn config = config { optimisations = nub newopt }
                  where opt = optimisations config 
                        newopt = if isChecked then tok:opt else delete tok opt
  modifyIORef pst (\x -> x{pa = fn (pa x)})
  return ()
\end{code}
 
% --------------------------------------------------------------------
\section{Loading files}
% --------------------------------------------------------------------

\paragraph{gramsemBrowser}: creates the popup window when you press 
the Load files button.  Allows for changing grammar and semantics files.
TODO: respond to the Enter key select the text?

\begin{code}
gramsemBrowser :: (Textual a, Visible a, Textual b) => PState -> (a,a,a,b) -> IO ()
gramsemBrowser pst guiParts = do
  mst <- readIORef pst
  let config = pa mst
      tfile = tsFile config 
      gtype = grammarType config
      gfile = macrosFile config
      lfile = lexiconFile config
  f <- frame [text := "Grammar and semantics", clientSize := sz 400 150]
  -- Grammar files selection
  entryG  <- textEntry f AlignLeft [ text := trim gfile ]
  fselBtG <- button f [ text := "Browse"
                      , on command := newFileSel f entryG  ]
  entryL  <- textEntry f AlignLeft [ text := trim lfile,
                                     enabled := (gtype == GeniHand) ]
  fselBtL <- button f [ text := "Browse" 
                      , on command := newFileSel f entryL ]
  -- Target semantics file selection
  entryS   <- textEntry f AlignLeft [ text := trim tfile ]
  fselBtS <- button f [ text := "Browse"
                      , on command := newFileSel f entryS ]
  -- Grammar type selection
  let rlabels = ["handwritten", "TAGMLish"]
  rBox <- radioBox f Horizontal rlabels [ text := "Grammar type"
                                        , selection := if (gtype == TAGML) then 1 else 0
                                        ]
  -- Cancel button
  cancelBt <- button f [ text := "Cancel" , on command := close f ]
  -- Load button
  let (gl,ll,tl,tsBox) = guiParts
  let loadCmd reload = do -- get new values
                         teG' <- get entryG text
                         teL' <- get entryL text
                         teS' <- get entryS text
                         let teG = trim teG'
                             teL = trim teL'
                             teS = trim teS'
                         -- what kind of grammar does the user want?
                         gramSel <- get rBox selection
                         let newGtype = if (gramSel == 0) 
                                        then GeniHand 
                                        else TAGML
                         -- write the new values
                         let newPa p = p { macrosFile  = teG 
                                         , lexiconFile = teL 
                                         , grammarType = newGtype 
                                         , tsFile = teS }
                         modifyIORef pst (\x -> let pa' = pa x in x{pa = newPa pa'})
                         -- load in any new files
                         Monad.when (reload || teG /= gfile ) $ 
                           do loadMacros pst  
                              modifyIORef pst (\x -> x { tags = emptyFM }) 
                              set gl [ text := teG ]
                         Monad.when (reload || teL /= lfile ) $ 
                           do loadLexicon pst
                              set ll [ text := teL ]
                         Monad.when (reload || teS /= tfile ) $ 
                           do readTargetSem pst tsBox
                              set tl [ text := teS ]
                         close f 
  loadBt   <- button f [ text := "Load" 
                       , on command := loadCmd False ]
  -- a reload button is useful for people who edit the grammar while playing
  -- with the generator  
  reloadBt <- button f [ text := "Reload" 
                       , on command := loadCmd True ]
  -- Pack it all together.
  set f [layout := column 5 
              -- Grammar type
              [ hfill $ widget rBox
              -- Grammar/macros selection 
              , hfill $ row 5 [ label "trees  ", hfill $ widget entryG, widget fselBtG ]
              -- Lexicon selection 
              , hfill $ row 5 [ label "lexicon", hfill $ widget entryL, widget fselBtL ]
              -- Semantics
              , hfill $ row 5 [ label "input sem", hfill $ widget entryS, widget fselBtS ]
              -- Load button 
              , hfloatRight $ row 5 [ widget cancelBt, widget loadBt, widget reloadBt ]
              ] ]
\end{code}

\paragraph{newFileSel} Create a file selection dialog that gets its initial
filename from the given entry and writes the result back to the entry 

\begin{code}
newFileSel :: (Window a) -> (TextCtrl b) -> IO ()
newFileSel f ety = 
    do filename' <- get ety text
       let filename = trim filename'
       fsel <- fileOpenDialog f True True "Choose your file..." [] "" filename
       case fsel of
        -- if the user does not select any file there are no changes
        Nothing       -> return () 
        --
        Just file     -> set ety [ text := file ]
\end{code}

\paragraph{loadTargetSem} Loads but does not parse the target semantics. 
This is used when you click on "Reload target semantics" or if you load
a new target semantics.

\begin{code}
readTargetSem :: (Textual a) => PState -> a -> IO ()
readTargetSem pst tsBox= do 
  mst <- readIORef pst
  t   <- (readFile.tsFile.pa) mst 
  set tsBox [ text := trim t ]
\end{code}
   
% --------------------------------------------------------------------
\section{Results}
\label{sec:results_gui}
% --------------------------------------------------------------------

\paragraph{doGenerate} parses the target semantics, then calls the
generator and displays the result in a results gui (below).

\begin{code}
doGenerate :: Textual b => Window a -> PState -> b -> Bool -> IO ()
doGenerate f pst sembox debugger = do 
  let handler title err = errorDialog f title (show err)
      handler1 err = handler "Error (probably the target semantics): " err 
      -- handler2 err = do handler "Error during generation:" err
                        --return GR { grCand = [], grAuts = [], grCombos = [], 
                        --grFinalAut = emptyaut, grDerived  = [], grStats    = ""}
  sem <- get sembox text
  let dodebug = do loadTargetSemStr pst sem
                   customGeni pst debugGui 
                   return ()
      dogen = do loadTargetSemStr pst sem
                 res <- customGeni pst runGeni 
                 mst <- readIORef pst
                 resultsGui res mst 
  -- FIXME: it would be nice to distinguish between generation and ts
  -- parsing errors
  (if debugger then dodebug else dogen) `catch` handler1
\end{code}

\paragraph{resultsGui} 

Results displays generation result in a window.  The window consists of
various tabs for intermediary results in lexical
selection, derived trees, derivation trees and generation statistics.

\begin{code}
resultsGui :: GeniResults -> State -> IO () 
resultsGui res mst = do
  let config = pa mst
      tsem   = ts mst
  -- results window
  f <- frame [ text := "Results" 
             , fullRepaintOnResize := False 
             , layout := stretch $ label "Generating..."
             , clientSize := sz 300 300 
             ] 
  p    <- panel f []
  nb   <- notebook p []
  -- candidate selection tab
  let cand = grCand res
  canTab <- candidateGui nb cand 
  -- automata tab
  let (candLite, _) = reduceTags (polsig config) cand
      extraPol = extrapol config 
      (auts, finalaut) = makePolAut candLite tsem extraPol
  autTab <- if (polarised config) 
            then polarityGui nb auts finalaut
            else messageGui  nb "Polarities disabled"
  -- realisations tab
  resTab <- realisationsGui nb $ grDerived res
  -- statistics tab
  statTab <- messageGui nb $ show res 
  -- pack it all together 
  set f [ layout := container p $ column 0 [ tabs nb 
               [ tab "lex selection" canTab
               , tab "automata"      autTab
               , tab "realisations"  resTab 
               , tab "summary"       statTab
               ] ]
        , clientSize := sz 700 600 
        ] 
  return ()
\end{code}

\subsection{Lexically selected items}

We have a browser for the lexically selected items.  We group the lexically
selected items by the semantics they subsume, inserting along the way some
fake trees and labels for the semantics.

\begin{code}
candidateGui :: (Window a) -> [TagElem] -> IO Layout
candidateGui f xs = 
  tagBrowserGui f xs "lexically selected item" "candidates"
\end{code}
      
\subsection{Polarity Automata}

A browser to see the automata constructed during the polarity optimisation
step.

\begin{code}
polarityGui :: (Window a) -> [(String,PolAut,PolAut)] -> PolAut -> IO Layout
polarityGui   f xs final = do
  let aut2  (_ , a1, a2) = [ a1, a2 ]
      autLabel (fv,_,_) = [ (show fv), (show fv ++ " pruned") ]
      autlist = map toGvPolAut $ concatMap aut2 xs ++ [ final ] 
      labels  = concatMap autLabel xs ++ [ "final" ]
      --
      itNlabl  = zip autlist labels
      tip      = "automata"
  (lay,_) <- graphvizGui f tip "polarity" itNlabl
  return lay
\end{code}
      
\subsection{Derived Trees}

Browser for derived/derivation trees, except if there are no results, we show a
message box

\begin{code}
realisationsGui :: (Window a) -> [TagElem] -> IO Layout
realisationsGui f [] = messageGui f "No results found"
realisationsGui f results = do
  let sentences = map showLeaves results
      itNlabl   = zip results sentences
      tip       = "result"
  (lay,_) <- tagViewerGui f tip "derived" itNlabl
  return lay
\end{code}

\subsection{TAG viewer and browser}

A TAG browser is a TAG viewer (see below) that groups trees by 
their semantics.

\begin{code}
tagBrowserGui :: (Window a) -> [TagElem] -> String -> String -> IO Layout
tagBrowserGui f xs tip cachedir = do 
  let semmap   = mapBySem tsemantics xs
      sem      = keysFM semmap
      --
      lookupTr   = lookupWithDefaultFM semmap []
      treesfor k = emptyTE : (lookupTr k)
      labsfor  k = ("___" ++ showPred k ++ "___") : (map fn $ lookupTr k)
                   where fn t = idname t 
      --
      trees    = concatMap treesfor sem
      labels   = concatMap labsfor  sem
      itNlabl  = zip trees labels
  (lay,_) <- tagViewerGui f tip cachedir itNlabl
  return lay
\end{code}
      
A TAG viewer is a graphvizGui that lets the user toggle the display
of TAG feature structures.

\begin{code}
tagViewerGui :: (Window a) -> String -> String -> [(TagElem,String)] 
               -> (IO (Layout, ([(TagElem,String)] -> Int -> IO ())))
tagViewerGui f tip cachedir itNlab = do
  p <- panel f []      
  (lay,updaterFn) <- graphvizGui p tip cachedir itNlab
  detailsChk <- checkBox p [ text := "Show features"
                           , checked := False ]
  -- commands
  let (trees,labels)  = unzip itNlab
      showTrees d = zip newTrees labels 
                    where newTrees = map fn trees
                          fn t   = t { showfeats = d }
  let onDetailsChk = do isDetailed <- get detailsChk checked 
                        updaterFn (showTrees isDetailed) (-1)
  -- handlers
  set detailsChk [ on command := onDetailsChk ]
  -- pack it all in      
  let cmdBar = hfill $ row 5 [ dynamic $ widget detailsChk ]
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
treeBrowserGui :: PState -> IO () 
treeBrowserGui pst = do
  mst <- readIORef pst
  -- ALL THE TREES in the grammar... muahahaha!
  let preCombined = tags mst
      semmap = if (isEmptyFM preCombined) 
               then combine (gr mst) (le mst)
               else preCombined 
  -- browser window
  f <- frame [ text := "Tree Browser" 
             , fullRepaintOnResize := False 
             ] 
  -- the heavy GUI artillery
  let sem      = keysFM semmap
      --
      lookupTr   = lookupWithDefaultFM semmap [] 
      treesfor k = emptyTE : (lookupTr k)
      labsfor  k = ("___" ++ k ++ "___") : (map fn $ lookupTr k)
                   where fn    t = idname t ++ polfn (tpolarities t)
                         polfn p = if isEmptyFM p 
                                   then "" 
                                   else " (" ++ showLitePm p ++ ")"
      --
      trees    = concatMap treesfor sem
      itNlabl  = zip trees (concatMap labsfor sem)
  (browser,_) <- tagViewerGui f "tree browser" "grambrowser" itNlabl
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
% --------------------------------------------------------------------

This creates an iteractive version of the generator that shows the
user the agenda, chart and results at various stages in the generation
process.  

\begin{code}
debugGui :: Params -> Sem -> [[TagElem]] -> IO ([TagElem], Gstats)
debugGui config tsem combos = do
  f <- frame [ text := "Geni Debugger" 
             , fullRepaintOnResize := False 
             , clientSize := sz 300 300 
             ] 
  p    <- panel f []
  nb   <- notebook p []
  -- create an information tab (FIXME: this is mostly a hack to hide
  -- a GUI bug with the first debugger tab under Linux) 
  pinfo <- panel nb []
  let infoText = "Input Semantics: " ++ showSem tsem
      infoLay  = fill $ container pinfo $ margin 1 $ label infoText
      infoTab  = tab "info" infoLay
  -- candidate selection tab
  let cand = concat combos
  canTab <- candidateGui nb cand 
  -- start the generator for each path
  let tabLabels = map (\x -> "session " ++ show x) [1..] 
      createTab (cd,xs) = debuggerTab nb config tsem cd xs
  debugTabs <- mapM createTab $ zip tabLabels combos
  let genTabs = map fn $ zip tabLabels debugTabs
                where fn (l,t) = tab l t
  --
  set f [ layout := container p $ column 0 
          [ tabs nb (infoTab
                    :(tab "lexical selection" canTab)
                    :genTabs) ]   
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
      initSt  = initMState cands [] tsem config
  let itNlabl   = showGenState False initRes initSt 
      tip       = "debugger session"
  -- widgets
  p <- panel f []      
  (lay,updaterFn) <- graphvizGui p tip cachedir itNlabl
  detailsChk <- checkBox p [ text := "Show features"
                           , checked := False ]
  restartBt   <- button p [text := "Start over"]
  nextBt   <- button p [text := "Leap by..."]
  leapVal  <- textEntry p AlignLeft [ text := "1", clientSize := sz 30 25 ]
  finishBt <- button p [text := "Continue"]
  statsTxt <- staticText p []
  -- commands
  let updateStatsTxt gs = set statsTxt [ text :~ (\_ -> txtStats gs) ]
      txtStats   gs =  "itr " ++ (show $ geniter gs) ++ " " 
                    ++ "chart sz: " ++ (show $ szchart gs) 
                    ++ "\ncomparisons: " ++ (show $ numcompar gs)
  let onDetailsChk r s sel = do isDetailed <- get detailsChk checked 
                                updaterFn (showGenState isDetailed r s) sel
                                let cmd _ = onDetailsChk r s (-1) 
                                set detailsChk [ on command :~ cmd ]
  let genStep _ (r2,s2) = (r2 ++ r3,s3)
                          where (r3,s3) = runState generateStep s2
  let showNext r s = do leapTxt <- get leapVal text
                        let leapInt = read leapTxt
                        let (r2,s2) = foldr genStep (r,s) [1..leapInt]
                        onDetailsChk r2 s2 1
                        updateStatsTxt (genstats s2)
                        set nextBt [ on command :~ (\_ -> showNext r2 s2) ]
  let showLast = do -- redo generation from scratch
                    let (r,s) = runState generate initSt 
                    onDetailsChk r s (-1)
                    updateStatsTxt (genstats s)
  let showReset = do let res = initRes 
                         st  = initSt 
                     set nextBt   [ on command  := showNext res st ]
                     updateStatsTxt initGstats
                     onDetailsChk res st 1
  -- handlers
  set detailsChk [ on command := onDetailsChk initRes initSt (-1) ]
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
showGenState :: Bool -> [TagElem] -> Mstate -> [(TagElem,String)]
showGenState detailed res st = 
  let agenda    = initrep st
      auxiliary = auxrep st
      chart     = genRepToList $ genrep  st
      --
      trees'     =  (emptyTE:agenda) ++ (emptyTE:chart) 
                 ++ (emptyTE:auxiliary) ++ (emptyTE:res) 
      trees      = map (\x -> x { showfeats = detailed }) trees'
      labels     =  ("___AGENDA___"    : (labelFn agenda))
                 ++ ("___CHART___"     : (labelFn chart))
                 ++ ("___AUXILIARY___" : (labelFn auxiliary))
                 ++ ("___RESULTS___"   : (labelFn res)) 
      labelFn trs = map fn trs 
                    where fn t = showLeaves t ++ " (" ++ showPolPaths t ++ ")"
  in zip trees labels
\end{code}

% --------------------------------------------------------------------
\section{Graphviz GUI}
\label{sec:graphviz_gui}
% --------------------------------------------------------------------

A general-purpose GUI for displaying a list of items graphically
via AT\&T's excellent Graphviz utility.  This returns a layout
(wxhaskell container) and a function for updating the contents
of this GUI.

We have a list box where we display all the labels the user provided.
If the user selects an entry from this box, then the item corresponding
to that label will be displayed.  See section \ref{sec:draw_item}.

Arguments:
\begin{enumerate}
\item f - (parent window) the GUI is provided as a panel within the parent.
          Note: we use window in the WxWidget's sense, meaning it could be
          anything as simple as a another panel, or a notebook tab.
\item glab - (gui labels) a tuple of strings (tooltip, next button text)
\item cachedir - the cache subdirectory.  We intialise this by creating a cache
          directory for images which will be generated from the results
\item itNlab - (items and labels) a list of pairs of $(it,lab)$ where $it$
          is an item to visualise and $lab$ is its lable.
\end{enumerate}

Returns: a function for updating the GUI 
(args for the updater function are itNlab and the index you want to select or
 -1 to keep the same selection)

\begin{code}
graphvizGui :: (GraphvizShow d) => (Window a) -> String -> String 
               -> [(d,String)] 
               -> (IO (Layout, ([(d,String)] -> Int -> IO ())))
graphvizGui f txtChoiceTip cachedir itNlab = do
  -- widgets
  p <- panel f [ fullRepaintOnResize := False ]
  split <- splitterWindow p []
  (dtBitmap,sw) <- scrolledBitmap split 
  rchoice  <- singleListBox split False 
              [selection := 1, tooltip := txtChoiceTip]
  -- set handlers
  let openFn   = openImage sw dtBitmap 
  -- pack it all together
  let lay = fill $ container p $ margin 1 $ fill $ 
            vsplit split 5 200 (widget rchoice) (widget sw) 
  set p [ on closing :~ \previous -> do{ closeImage dtBitmap; previous } ]
  -- create an updater function
  let updaterFn itNlab2 newSel = do 
        initCacheDir cachedir 
        let (it,labels) = unzip itNlab2
            drawables   = array (0, length it) (zip [0..] it)
            showItem = do { sel <- get rchoice selection; 
                            createAndOpenImage cachedir p sel drawables openFn }
        sel' <- get rchoice selection;
        let sel = if (newSel < 0) 
                  then if (sel' < 0) then 0 else sel'
                  else newSel
        set rchoice [ items :~ (\_ -> labels), selection :~ (\_ -> sel), 
                      on select :~ (\_ -> showItem) ]
        showItem 
  -- call the updater function for the first time
  updaterFn itNlab 1
  -- return a layout and the updater function 
  return (lay, updaterFn)
\end{code}

\subsection{Scroll bitmap}

Bitmap with a scrollbar

\begin{code}
scrolledBitmap :: Window a -> IO(VarBitmap, ScrolledWindow ())
scrolledBitmap p = do
  dtBitmap <- varCreate Nothing
  sw       <- scrolledWindow p [scrollRate := sz 10 10, --bgcolor := white,
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
    varSet vbitmap (Just bm)
    -- set status [text := fname]
    -- reset the scrollbars 
    bmsize <- bitmapGetSize bm
    set sw [virtualSize := bmsize]
    --refit sw
    repaint sw
      `catch` \_ -> repaint sw

closeImage :: VarBitmap -> IO ()
closeImage vbitmap = do 
    mbBitmap <- varSwap vbitmap Nothing
    case mbBitmap of
        Nothing -> return ()
        Just bm -> bitmapDelete bm
\end{code}

\begin{code}
onPaint :: VarBitmap -> DC a -> b -> IO ()
onPaint vbitmap dc _ = do 
    mbBitmap <- varGet vbitmap
    case mbBitmap of
      Nothing -> dcClear dc
      Just bm -> drawBitmap dc bm pointZero False []
\end{code}

\subsection{Drawing stuff}
\label{sec:draw_item}

\paragraph{createAndOpenImage} Attempts to draw an image 
(or retrieve it from cache) and opens it if we succeed.  Otherwise, it
does nothing at all; the creation function will display an error message
if it fails.

\begin{code}
createAndOpenImage :: (GraphvizShow b) => 
  FilePath -> Window a -> Int -> Array Int b -> OpenImageFn -> IO ()
createAndOpenImage cachedir f sel drawables openFn = do 
  r <- createImage cachedir f sel drawables 
  case r of 
    Just graphic -> openFn graphic
    Nothing -> return ()
\end{code}

\paragraph{createImage}
Creates a graphical visualisation for a tree or automaton.
arguments: a drawing function, an array of trees and an index 
Returns Just filename if the index is valid or Nothing otherwise 

\begin{code}
createImage :: (GraphvizShow b) => FilePath -> Window a -> Int -> Array Int b -> IO (Maybe FilePath) 
createImage cachedir f sel drawables = do
  --putStrLn $ "creating image for " ++ (show sel) ++ " in " ++ (show trees)
  let te = drawables ! sel
      b  = bounds drawables 
      dotFile     = createDotPath cachedir (show sel)
      graphicFile = createImagePath cachedir (show sel)
      create = do toGraphviz te dotFile graphicFile
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
  return (fill $ container p $ margin 10 $ fill $ column 1 $ [ label msg ]) 
\end{code}

\begin{code}
trim :: String -> String
trim = reverse . (dropWhile isSpace) . reverse . (dropWhile isSpace) 
\end{code}

\begin{code}
gv_CACHEDIR :: String
gv_CACHEDIR = "gvcache"

-- FIXME: should use OS-independant seperator
createImagePath :: String -> String -> String
createImagePath subdir name = 
  gv_CACHEDIR ++ "/" ++ subdir ++ "/" ++ name ++ ".png"

createDotPath :: String -> String -> String
createDotPath subdir name = 
  gv_CACHEDIR ++ "/" ++ subdir ++ "/" ++ name ++ ".dot"
\end{code}


