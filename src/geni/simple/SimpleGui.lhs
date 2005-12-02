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

\begin{code}
module SimpleGui(guiGenerate) where
\end{code}

\ignore{
\begin{code}
import Graphics.UI.WX
import Graphics.UI.WXCore

import qualified Control.Monad as Monad 
import qualified Data.Map as Map

import Control.Monad.State (runState)
import Data.Array
import Data.IORef
import Data.List (isPrefixOf, find, intersperse, nub, delete, (\\))
import Data.Maybe (isJust)
import System.Directory 
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.Process (runProcess)
import Text.ParserCombinators.Parsec ( runParser )

import Graphviz 
import Treeprint(graphvizShowTagElem)

import Tags (tagLeaves)
import Geni (ProgState(..), GeniInput(..), GeniResults(..), ProgStateRef,
             doGeneration, runGeni, 
             combine, loadGrammar, loadTestSuite, loadTargetSemStr)
import General (trim, snd3, slash, bugInGeni)
import Btypes 
  (showPred, showSem, showPairs, showLexeme,
   Sem, iword, isemantics)
import Tags (idname,mapBySem,emptyTE,tsemantics,tpolarities,thighlight, 
             TagElem, derivation)

import Configuration(Params(..), Switch(..), GrammarType(..),
                     polarised, polsig, chartsharing, 
                     semfiltered, footconstr)
import GeniParsers 
import Gui

import qualified Builder as B
import Polarity
import SimpleBuilder 
  (simpleBuilder, Mstate, genconfig, theAgenda, theAuxAgenda, theChart, theTrash)
\end{code}
}

% --------------------------------------------------------------------
\section{Results}
\label{sec:results_gui}
% --------------------------------------------------------------------

\paragraph{doGenerate} parses the target semantics, then calls the
generator and displays the result in a results gui (below).

\begin{code}
doGenerate :: Textual b => Window a -> ProgStateRef -> b -> Bool -> IO ()
doGenerate f pstRef sembox debugger = do 
  loadGrammar pstRef
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

The arguments \fnparam{missedSem} and \fnparam{missedLex} are used to 
indicate to the user respectively if any bits of the input semantics
have not been accounted for, or if there have been lexically selected
items for which no tree has been found.

\begin{code}
candidateGui :: ProgState -> (Window a) -> [TagElem] -> Sem -> [String] -> IO Layout
candidateGui pst f xs missedSem missedLex = do
  p  <- panel f []      
  tb <- tagBrowserGui pst p xs "lexically selected item" "candidates"
  let warningSem = if null missedSem then ""
                   else "WARNING: no lexical selection for " ++ showSem missedSem ++ "\n"
      warningLex = if null missedLex then ""
                   else "WARNING: '" ++ (concat $ intersperse ", " missedLex) 
                        ++ "' were lexically selected, but are not anchored to"
                        ++ " any trees\n"
      warning = warningSem ++ warningLex
      items = if null warning then [ fill tb ] else [ hfill (label warning) , fill tb ]
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
  do let tip = "result"
         itNlabl = map (\t -> (noHighlight t, toSentence t)) resultsRaw
            where noHighlight x = x { thighlight = [] }
     --
     pst     <- readIORef pstRef
     (lay,_) <- tagViewerGui pst f tip "derived" itNlabl
     return lay
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
  let config = pa pst
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
  let onDisplayTrace 
       = do gvSt <- readIORef gvRef
            s <- get displayTraceCom selection
            let tsel = gvsel gvSt
            Monad.when (boundsCheck tsel tagelems) $ do
            let tree = tagelems !! (gvsel gvSt)
                derv = extractDerivation tree
            if (boundsCheck s derv)
               then runViewTag pst (derv !! s)
               else fail $ "Gui: bounds check in onDisplayTrace\n" ++ bugInGeni
  let onDetailsChk c 
       = do isDetailed <- get c checked 
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
debugGui :: ProgState -> GeniInput -> IO ([TagElem], B.Gstats)
debugGui pst input = do
  let tsem = giSem input
  --
  f <- frame [ text := "Geni Debugger" 
             , fullRepaintOnResize := False 
             , clientSize := sz 300 300 
             ] 
  p    <- panel f []
  nb   <- notebook p []
  -- candidate selection tab
  let cand    = giCands input
      candsem = (nub $ concatMap tsemantics cand)
      missedSem  = tsem \\ candsem
      --
      lexonly   = giLex input
      -- we assume that for a tree to correspond to a lexical item,
      -- it must have the same semantics
      hasTree l = isJust $ find (\t -> tsemantics t == lsem) cand
        where lsem = isemantics l
      missedLex = [ showLexeme (iword l) | l <- lexonly, (not.hasTree) l ]
  canTab <- candidateGui pst nb cand missedSem missedLex
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
  return ([], B.initGstats)
\end{code}

The generation could conceivably be broken into multiple generation
tasks, so we create a separate tab for each task.

\begin{code}
debuggerTab :: (Window a) -> Params -> Sem -> String -> [TagElem] -> IO Layout 
debuggerTab f config tsem cachedir cands = do
  let initBuilder = B.init  simpleBuilder
      nextStep    = B.step  simpleBuilder
      runBuilder  = B.run   simpleBuilder
      genstats    = B.stats simpleBuilder
      --
      initRes = []
      initSt  = initBuilder tsem cands (config {usetrash=True})
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
  leapVal  <- entry p [ text := "1", clientSize := sz 30 25 ]
  finishBt <- button p [text := "Continue"]
  statsTxt <- staticText p []
  -- commands
  let updateStatsTxt gs = set statsTxt [ text :~ (\_ -> txtStats gs) ]
      txtStats   gs =  "itr " ++ (show $ B.geniter gs) ++ " " 
                    ++ "chart sz: " ++ (show $ B.szchart gs) 
                    ++ "\ncomparisons: " ++ (show $ B.numcompar gs)
  let onDetailsChk = do isDetailed <- get detailsChk checked 
                        setGvParams gvRef isDetailed
                        updaterFn
  let genStep _ (r2,s2) = (r2 ++ r3,s3)
        where (r3,s3) = runState nextStep s2
  let showNext r s = do leapTxt <- get leapVal text
                        let leapInt = read leapTxt
                            (r2,s2) = foldr genStep (r,s) [1..leapInt]
                        setGvDrawables2 gvRef (showGenState r2 s2)
                        setGvSel gvRef 1
                        updaterFn
                        updateStatsTxt (genstats s2)
                        set nextBt [ on command :~ (\_ -> showNext r2 s2) ]
  let showLast = do -- redo generation from scratch
                    let (r,s) = runState runBuilder initSt 
                    setGvDrawables2 gvRef (showGenState r s)
                    updaterFn
                    updateStatsTxt (genstats s)
  let showReset = do let res = initRes 
                         st  = initSt 
                     set nextBt   [ on command  := showNext res st ]
                     updateStatsTxt (B.initGstats)
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
  let agenda    = theAgenda st
      auxiliary = theAuxAgenda st
      trash     = theTrash st
      chart     = theChart  st
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
      --
      showPaths = if (chartsharing $ genconfig st)
                  then (\t -> " (" ++ showPolPaths t ++ ")")
                  else const ""
      labelFn trs = map (\t -> (toSentence t) ++ (showPaths t)) trs 
  in (trees,labels)
\end{code}

\subsection{XMG Metagrammar stuff}

XMG trees are produced by the XMG metagrammar system
(\url{http://sourcesup.cru.fr/xmg/}). To debug these grammars, it is
useful, given a TAG tree, to see what its metagrammar origins are.  We
provide here an interface to the handy visualisation tool ViewTAG that
just does this.

\paragraph{extractDerivation} retrieves the names of all the
XMG trees that went to building a TagElem, including the TagElem
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
     let extractXMGName n = tail $ dropWhile (/= '_') n 
         drName = extractXMGName idname 
     -- run the viewer 
     let cmd  = viewCmd params 
         args = [gramfile, drName]
     -- run the viewer
     runProcess cmd args Nothing Nothing Nothing Nothing Nothing
     return ()
\end{code}
