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

\chapter{CKY Gui}

\begin{code}
module CkyGui where
\end{code}

\ignore{
\begin{code}
import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (when)

import Control.Exception (assert)
import qualified Control.Monad as Monad 
import Control.Monad (liftM)

import Data.Array ((!))
import Data.IORef
import Data.List (find, intersperse, nub, (\\))
import qualified Data.Map as Map 
import Data.Maybe (listToMaybe, isJust, catMaybes)
import Data.Tree 
import Text.ParserCombinators.Parsec (parseFromFile)

import Automaton
 ( NFA(states, transitions, startSt, finalStList)
 , lookupTrans, addTrans )
import qualified Builder    as B
import qualified BuilderGui as BG 
import Btypes
  ( GNode, gnname, showLexeme, iword, isemantics, Sem )

import CkyBuilder 
  ( ckyBuilder, CkyStatus, CkyItem(..), ChartId
  , ciRoot, ciAdjDone
  , bitVectorToSem, findId,
  , extractDerivations
  , theResults, theAgenda, theChart, theTrash
  , emptySentenceAut, mJoinAutomata, mAutomatonPaths,
  , unpackItemToAuts,
  )
import Configuration ( Params(..), polarised )

import Geni 
  ( ProgState(..), ProgStateRef
  , initGeni, runGeni )
import General ( boundsCheck, listRepNode, geniBug, fst3 )
import GeniParsers ( geniTagElems )
import Graphviz 
  ( GraphvizShow(..), gvNode, gvEdge, gvSubgraph, gvUnlines, gvShowTree
  , gvNewline
  , GraphvizShowNode(..) )
import GuiHelper 
  ( candidateGui, sectionsBySem, messageGui, polarityGui, toSentence
  , debuggerPanel, DebuggerItemBar
  , addGvHandler, modifyGvParams, 
  , GraphvizGuiSt(gvitems, gvsel), GvIO
  , maybeSaveAsFile
  , statsGui, graphvizGui, newGvRef, setGvDrawables, setGvDrawables2,
  )

import Polarity
import Statistics (Statistics)
import Tags 
  ( idname, tsemantics, ttree, TagElem )
import Treeprint ( toGeniHand )
\end{code}
}

% --------------------------------------------------------------------
\section{Interface}
% --------------------------------------------------------------------

\begin{code}
ckyGui = BG.BuilderGui {
      BG.generateGui = generateGui 
    , BG.debugGui = debugGui }

generateGui :: ProgStateRef -> IO ()
generateGui pstRef =
  runGeni pstRef ckyBuilder >>= resultsGui pstRef
\end{code}

% --------------------------------------------------------------------
\section{Results}
\label{sec:cky_results_gui}
% --------------------------------------------------------------------

The results gui displays two kinds of information: a detailed results
browser showing the chart items that are classified as results, and a
summary showing the list of sentences and some statistical stuff.

\begin{code}
resultsGui :: ProgStateRef -> ([String], Statistics, CkyStatus) -> IO ()
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

-- | Browser for the results (if there are any)
realisationsGui :: ProgStateRef -> (Window a) -> [CkyItem]
                -> GvIO CkyDebugParams (Maybe CkyItem)
realisationsGui _ f [] =
  do m <- messageGui f "No results found"
     gvRef <- newGvRef initCkyDebugParams [] ""
     return (m, gvRef, return ())
realisationsGui _ f resultsRaw =
  do let tip = "result"
         results = map Just resultsRaw
         labels  = map (toSentence.ciSourceTree) resultsRaw
     gvRef <- newGvRef initCkyDebugParams labels tip
     setGvDrawables gvRef results
     graphvizGui f "cky-results" gvRef
\end{code}

% --------------------------------------------------------------------
\section{Debugger}
\label{sec:cky_debugger_gui}
\label{fn:ckyDebugGui}
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
    f <- frame [ text := "GenI Debugger - CKY edition" 
               , fullRepaintOnResize := False 
               , clientSize := sz 300 300 ] 
    p    <- panel f []
    nb   <- notebook p []
    -- generation step 1
    initStuff <- initGeni pstRef
    let (tsem,_)   = B.inSemInput initStuff
        (cand,_)   = unzip $ B.inCands initStuff
        lexonly    = B.inLex initStuff 
    -- continuation for candidate selection tab
    let step2 newCands =
         do -- generation step 2.A (run polarity stuff)
            let newInitStuff = initStuff { B.inCands = map (\x -> (x, -1)) newCands } 
                (combos, autstuff, input2) = B.preInit newInitStuff config
                cands2PP = assert (length combos == 1) $ head combos
            -- automata tab
            let (auts, finalaut, _) = autstuff
            autPnl <- if polarised config
                      then fst3 `liftM` polarityGui nb auts finalaut
                      else messageGui nb "polarities disabled"
            -- generation step 2.B (start the generator for each path)
            debugPnl <- ckyDebuggerTab nb config (input2 { B.inCands = cands2PP }) "cky"
            let autTab   = tab "automata" autPnl
                debugTab = tab "session" debugPnl
                genTabs  = if polarised config then [ debugTab ] else [ autTab, debugTab ]
            --
            set f [ layout := container p $ tabs nb genTabs
                  , clientSize := sz 700 600 ]
            return ()
    -- candidate selection tab
    let missedSem  = tsem \\ (nub $ concatMap tsemantics cand)
        -- we assume that for a tree to correspond to a lexical item,
        -- it must have the same semantics
        hasTree l = isJust $ find (\t -> tsemantics t == lsem) cand
          where lsem = isemantics l
        missedLex = [ showLexeme (iword l) | l <- lexonly, (not.hasTree) l ]
    (canPnl,_,_) <- ckyCandidateGui pst nb cand missedSem missedLex step2
    -- basic tabs 
    let basicTabs = [ tab "lexical selection" canPnl ]
    --
    set f [ layout := container p $ tabs nb basicTabs
          , clientSize := sz 700 600 ]
    return ()
\end{code}
  
The generation could conceivably be broken into multiple generation
tasks, so we create a separate tab for each task.

FIXME: when we've made this more sophisticated, we should refactor so that simple
can have access to the same functionality

\begin{code}
ckyCandidateGui :: ProgState -> (Window a) -> [TagElem] -> Sem -> [String] -> ([TagElem] -> IO ())
                -> GvIO Bool (Maybe TagElem)
ckyCandidateGui pst f xs missedSem missedLex job = do
  p <- panel f []
  candV <- varCreate xs
  (tb, ref, updater) <- candidateGui pst p xs missedSem missedLex
  -- supplementary button bar
  let saveCmd =
       do c <- varGet candV
          let cStr = unwords $ map toGeniHand c
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
                                setGvDrawables2 ref (unzip $ sectionsBySem c)
                                updater
  --
  saveBt <- button p [ text := "Save to file", on command := saveCmd ]
  loadBt <- button p [ text := "Load from file", on command := loadCmd ]
  nextBt <- button p [ text := "Continue" ]
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

\begin{code}
data CkyDebugParams = 
 CkyDebugParams { debugShowFeats       :: Bool 
                , debugWhichDerivation :: Int }

initCkyDebugParams = 
 CkyDebugParams { debugShowFeats       = False
                , debugWhichDerivation = 0 }

-- would be nice if Haskell sugared this kind of stuff for us
setDebugShowFeats :: Bool -> CkyDebugParams -> CkyDebugParams
setDebugShowFeats b x = x { debugShowFeats = b }

setDebugWhichDerivation :: Int -> CkyDebugParams -> CkyDebugParams
setDebugWhichDerivation w x = x { debugWhichDerivation = w }

ckyDebuggerTab :: (Window a) -> Params -> B.Input -> String -> IO Layout 
ckyDebuggerTab = debuggerPanel ckyBuilder initCkyDebugParams stateToGv ckyItemBar
 where 
  stateToGv st = 
   let agenda  = section "AGENDA"  $ theAgenda  st
       trash   = section "TRASH"   $ theTrash   st
       chart   = section "CHART"   $ theChart   st
       results = section "RESULTS" $ theResults st
       --
       section n i = hd : (map tlFn i)
         where hd = (Nothing, "___" ++ n ++ "___")
               tlFn x = (Just (st,x), labelFn x)
       showPaths = const ""
                   {- if (polarised $ genconfig st)
                      then (\t -> " (" ++ showPolPaths t ++ ")")
                      else const "" -}
       gorn i = case gornAddressStr (ttree $ ciSourceTree i) (ciNode i) of
                Nothing -> geniBug "A chart item claims to have a node which is not in its tree"
                Just x  -> x
       isComplete i = ciRoot i && ciAdjDone i
       -- try displaying as an automaton, or if all else fails, the tree sentence
       fancyToSentence ci =
        let mergedAut = uncurry mJoinAutomataUsingHole $ unpackItemToAuts st ci
            boringSentence = toSentence $ ciSourceTree ci
        in  case mAutomatonPaths mergedAut of
            []    -> boringSentence
            (h:_) -> unwords $ map fst $ h
       labelFn i = unwords [ completeStr ++ idStr ++ gornStr
                           , fancyToSentence i
                           , "/" ++ (idname $ ciSourceTree i)
                           , showPaths i
                           ]
         where idStr       = show $ ciId i
               completeStr = if isComplete i then ">" else ""
               gornStr     = if isComplete i then "" else " g" ++ (gorn i)
   in unzip $ agenda ++ chart ++ results ++ trash

ckyItemBar :: DebuggerItemBar CkyDebugParams (CkyStatus, CkyItem)
ckyItemBar f gvRef updaterFn =
 do ib <- panel f []
    -- select derivation
    derTxt    <- staticText ib []
    derChoice <- choice ib [ tooltip := "Select a derivation" ]
    let onDerChoice =
         do sel <- get derChoice selection
            modifyGvParams gvRef (setDebugWhichDerivation sel)
            updaterFn
    set derChoice [ on select := onDerChoice ]
    -- show features
    detailsChk <- checkBox ib [ text := "Show features"
                              , checked := False ]
    let onDetailsChk = 
         do isDetailed <- get detailsChk checked 
            modifyGvParams gvRef (setDebugShowFeats isDetailed)
            updaterFn
    set detailsChk [ on command := onDetailsChk ] 
    -- add a handler for when an item is selected: 
    -- update the list of derivations to choose from
    let updateDerTxt t = set derTxt [ text := "deriviations (" ++ t ++ ")" ]
        handler gvSt = 
         do case (gvitems gvSt ! gvsel gvSt) of 
             Nothing    -> 
               do set derChoice [ enabled := False, items := [] ]
                  updateDerTxt "n/a"
             Just (s,c) -> 
               do let derivations = extractDerivations s c 
                      dervLabels  = zipWith (\n _ -> show n) [1..] derivations 
                  set derChoice [ enabled := True, items := dervLabels, selection := 0 ]
                  onDerChoice
                  updateDerTxt $ show $ length derivations
    addGvHandler gvRef handler
    -- call the handler to react to the first selection
    handler `liftM` readIORef gvRef
    --
    return $ hfloatCentre $ container ib $ row 5 
                [ widget detailsChk 
                , hspace 5, widget derTxt,  rigid $ widget derChoice ] 
\end{code}

\section{Helper code}

\begin{code}

gornAddressStr :: Tree GNode -> GNode -> Maybe String
gornAddressStr t target =
  (concat . (intersperse ".") . (map show)) `liftM` gornAddress t target

gornAddress :: Tree GNode -> GNode -> Maybe [Int]
gornAddress t target = reverse `liftM` helper [] t
 where
 helper current (Node x _)  | (gnname x == gnname target) = Just current
 helper current (Node _ l)  = listToMaybe $ catMaybes $
                              zipWith (\c t -> helper (c:current) t) [1..] l

instance GraphvizShow CkyDebugParams (CkyStatus, CkyItem) where
  graphvizLabel  f (_,c) = graphvizLabel f c
  graphvizParams f (_,c) = graphvizParams f c
  graphvizShowAsSubgraph f p (s,c) = 
   let color x = ("color", x)
       label x = ("label", x)
       style x = ("style", x)
       arrowtail x = ("arrowtail", x)
       --
       substColor = color "blue"
       adjColor   = color "red"
       --
       edgeParams (_ ,"no-adj") = [ label "na" ]
       edgeParams (_, "kids"  ) = [ label "k" ]
       edgeParams (_, "init"  ) = [ label "i" ]
       edgeParams (_, "subst" ) = [ style "bold",         substColor, arrowtail "normal" ]
       edgeParams (_, "adj"   ) = [ style "bold, dashed", adjColor  , arrowtail "normal" ]
       edgeParams (_, "subst-finish") = [ substColor ]
       edgeParams (_, "adj-finish")   = [ adjColor, style "dashed" ]
       edgeParams (_, k) = [ ("label", "UNKNOWN: " ++ k) ]
       --
       whichDer    = debugWhichDerivation f
       derivations = extractDerivations s c
       showTree i t = gvSubgraph $ gvShowTree edgeParams s (p ++ "t" ++ (show i)) t
       gvDerv = if boundsCheck whichDer derivations
                then showTree whichDer (derivations !! whichDer)
                else geniBug $ "Bounds check failed on derivations selector:\n"
                               ++ "Asked to visualise: " ++ (show whichDer) ++ "\n"
                               ++ "Bounds: 0 to " ++ (show $ length derivations - 1)
       --
       joinedAut = uncurry mJoinAutomataUsingHole $ unpackItemToAuts s c
       gvAut     = graphvizShowAsSubgraph () (p ++ "aut")  joinedAut
       --
       showFeats  = debugShowFeats f
       treeParams = unlines $ graphvizParams showFeats $ ciSourceTree c
   -- FIXME: will have to make this configurable, maybe, show aut, show tree? radio button?
   in    "\n// ------------------- elementary tree --------------------------\n"
      ++ treeParams ++ graphvizShowAsSubgraph f p c
      ++ "\n// ------------------- automata (joined) ------------------------\n"
      ++ gvSubgraph gvAut
      ++ "\n// ------------------- derivations --------------------------\n"
      ++ treeParams ++ "node [ shape = plaintext, peripheries = 0 ]\n"
      ++ gvDerv

instance GraphvizShowNode CkyStatus (ChartId, String) where
  graphvizShowNode st prefix ci = 
   let theId = fst ci
       txt = case findId st theId of
             Nothing   -> ("???" ++ (show theId))
             Just item -> (show theId) ++ " " ++ (show.ciNode) item 
                          ++ " (" ++ ((idname.ciSourceTree) item) ++ ")"
   in gvNode prefix txt []

instance GraphvizShow CkyDebugParams CkyItem where
  graphvizLabel  f ci =
    graphvizLabel (debugShowFeats f, nullHlter) (toTagElem ci) ++
    gvNewline ++ (gvUnlines $ ciDiagnostic ci)

  graphvizShowAsSubgraph f prefix ci = 
   let showFeats = debugShowFeats f
       hlter n = (n, if (gnname n) == (gnname $ ciNode ci)
                     then Just "red" else Nothing)
   in  graphvizShowAsSubgraph (showFeats,hlter) (prefix ++ "tree")  $ toTagElem ci

nullHlter :: GNode -> (GNode, Maybe String)
nullHlter a = (a,Nothing)

toTagElem :: CkyItem -> TagElem
toTagElem ci =
 let node = ciNode ci
     te   = ciSourceTree ci
     --
     updateTree t = (head.fst) $ listRepNode replaceFn filtFn [t]
      where filtFn (Node a _)    = (gnname a == gnname node)
            replaceFn (Node _ k) = Node node k
     --
 in te { ttree = (updateTree.ttree) te
       , tsemantics  = bitVectorToSem (ciSemBitMap ci) (ciSemantics ci) }

-- FIXME: this is largely copy-and-pasted from Polarity.lhs 
-- it should be refactored later
instance GraphvizShow () B.SentenceAut where
  graphvizShowAsSubgraph _ prefix aut =
   let st  = (concat.states) aut
       ids = map (\x -> prefix ++ show x) [0..]
       -- map which permits us to assign an id to a state
       stmap = Map.fromList $ zip st ids
       lookupFinal x = Map.findWithDefault "error_final" x stmap
   in -- final states should be a double-edged ellispse
      "node [ peripheries = 2 ]; "
      ++ (unlines $ map lookupFinal $ finalStList aut)
      -- any other state should be an ellipse
      ++ "node [ shape = ellipse, peripheries = 1 ]\n"
      -- draw the states and transitions 
      ++ (concat $ zipWith gvShowState ids st) 
      ++ (concat $ zipWith (gvShowTrans aut stmap) ids st )

type SentenceAutState = Int 

gvShowState :: String -> SentenceAutState -> String
gvShowState stId st = gvNode stId (show st) []

gvShowTrans :: B.SentenceAut -> Map.Map SentenceAutState String
               -> String -> SentenceAutState -> String 
gvShowTrans aut stmap idFrom st = 
  let -- outgoing transition labels from st
      alpha = Map.keys $ Map.findWithDefault Map.empty st (transitions aut) 
      -- associate each st2 with a list of labels that transition to it
      inverter x fm = foldr fn fm (lookupTrans aut st x)
                      where fn    s f   = Map.insert s (xlist s f x) f
                            xlist s f x = x:(Map.findWithDefault [] s f)
      invFM = foldr inverter Map.empty alpha
      -- returns the graphviz dot command to draw a labeled transition
      drawTrans (stTo,x) = case Map.lookup stTo stmap of
                             Nothing   -> drawTrans' ("id_error_" ++ (show stTo)) x 
                             Just idTo -> drawTrans' idTo x
      drawTrans' idTo x = gvEdge idFrom idTo (drawLabel x) []
      drawLabel labels  = gvUnlines $ map fst $ catMaybes labels 
  in concatMap drawTrans $ Map.toList invFM
\end{code}

\begin{code}
-- | join two automata, inserting a ".." transition between them
mJoinAutomataUsingHole :: Maybe B.SentenceAut -> Maybe B.SentenceAut -> Maybe B.SentenceAut
mJoinAutomataUsingHole aut1 Nothing = aut1
mJoinAutomataUsingHole aut1 aut2 =
 mJoinAutomata aut1 $ mJoinAutomata (Just holeAut) aut2
 where holeAut = addTrans empty 0 (Just ("..",[])) 1
       empty   = emptySentenceAut { startSt = 0, finalStList = [1], states = [[0,1]] }
\end{code}
