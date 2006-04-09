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

import qualified Control.Monad as Monad 
import Control.Monad (liftM)

import Data.Array ((!), elems)
import Data.IORef
import Data.List (intersperse, findIndex, sort)
import qualified Data.Map as Map 
import Data.Maybe (listToMaybe, catMaybes)
import Data.Tree 

import Automaton
 ( NFA(states, transitions, startSt, finalStList)
 , lookupTrans, addTrans )
import qualified Builder    as B
import qualified BuilderGui as BG 
import Btypes
  ( GNode, gnname )

import CkyBuilder 
  ( ckyBuilder, earleyBuilder, CkyStatus, CkyItem(..), ChartId
  , ciRoot, ciAdjDone
  , bitVectorToSem, findId,
  , extractDerivations
  , theResults, theAgenda, theChart, theTrash
  , emptySentenceAut, mJoinAutomata, mAutomatonPaths,
  , unpackItemToAuts,
  )
import Configuration ( Params(..) )

import Geni 
  ( ProgStateRef, runGeni )
import General ( boundsCheck, geniBug )
import Graphviz 
  ( GraphvizShow(..), gvNode, gvEdge, gvSubgraph, gvUnlines, gvShowTree
  , gvNewline
  , GraphvizShowNode(..) )
import GuiHelper 
  ( messageGui, toSentence
  , debuggerPanel, DebuggerItemBar
  , addGvHandler, modifyGvParams, 
  , GraphvizGuiSt(gvitems, gvsel, gvparams), GvIO, setGvSel,
  , statsGui, graphvizGui, newGvRef, setGvDrawables,
  )

import Polarity
import Statistics (Statistics)
import Tags 
  ( idname, tsemantics, ttree, TagElem )
\end{code}
}

% --------------------------------------------------------------------
\section{Interface}
% --------------------------------------------------------------------

\begin{code}
ckyGui = BG.BuilderGui {
      BG.generateGui = generateGui False
    , BG.debuggerPnl = ckyDebuggerTab }

earleyGui = ckyGui { BG.generateGui = generateGui True }

generateGui :: Bool -> ProgStateRef -> IO ()
generateGui isEarley pstRef =
  runGeni pstRef builder >>= resultsGui pstRef
  where builder = if isEarley then earleyBuilder else ckyBuilder
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

\begin{code}
data CkyDebugParams = 
 CkyDebugParams { debugShowFeats       :: Bool 
                , debugShowFullDerv    :: Bool
                , debugShowSourceTree  :: Bool
                , debugWhichDerivation :: Int }

initCkyDebugParams = 
 CkyDebugParams { debugShowFeats       = False
                , debugShowFullDerv    = False
                , debugShowSourceTree  = False
                , debugWhichDerivation = 0 }

-- would be nice if Haskell sugared this kind of stuff for us
setDebugShowFeats, setDebugShowFullDerv :: Bool -> CkyDebugParams -> CkyDebugParams
setDebugShowFeats b x = x { debugShowFeats = b }
setDebugShowFullDerv b x = x { debugShowFullDerv = b }
setDebugShowSourceTree b x = x { debugShowSourceTree = b }

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
    jumpBtn <- button ib [ text := "Inspect" ]
    jumpChoice <- choice ib [ tooltip := "Jump to item." ]
    let onDerChoice =
         do sel <- get derChoice selection
            modifyGvParams gvRef (setDebugWhichDerivation sel)
            gvSt <- readIORef gvRef
            -- update the list of jump choices
            case (gvitems gvSt ! gvsel gvSt) of
             Just (s,c) -> do
               let t = selectedDerivation (gvparams gvSt) s c
                   nodes = map show $ sort $ derivationNodes t
               set jumpChoice [ items := nodes, selection := 0 ]
               updaterFn
             Nothing    -> return ()
    set derChoice [ on select := onDerChoice ]
    set jumpBtn [ on command := do
      gvSt <- readIORef gvRef
      jmpSel  <- get jumpChoice selection
      jmpItms <- get jumpChoice items
      let items = gvitems gvSt
          jmpTo = (read $ jmpItms !! jmpSel)
          isJmpTo Nothing  = False
          isJmpTo (Just (_,x)) = ciId x == jmpTo
      case findIndex isJmpTo (elems items) of
        Nothing -> geniBug $ "Was asked to see node " ++ (show jmpTo) ++ ", which is not in the list"
        Just x  -> setGvSel gvRef x
      onDerChoice
      updaterFn ]
    -- show features
    detailsChk <- checkBox ib [ text := "features"
                              , enabled := False, checked := False ]
    fullDervChk <- checkBox ib [ text := "full derivation"
                               , checked := False ]
    srcTreeChk <- checkBox ib [ text := "src tree"
                              , checked := False ]
    let setChkBoxUpdater box setter =
         set box [ on command := do isChecked <- get box checked
                                    modifyGvParams gvRef $ setter isChecked
                                    updaterFn ]
    setChkBoxUpdater detailsChk setDebugShowFeats
    setChkBoxUpdater fullDervChk setDebugShowFullDerv
    setChkBoxUpdater srcTreeChk setDebugShowSourceTree
    -- make detailsChk conditioned on srcTreeChk
    set srcTreeChk [ on command :~ \x -> x >> do
                      isChecked <- get srcTreeChk checked
                      set detailsChk [ enabled := isChecked ]
                   ]
    -- add a handler for when an item is selected: 
    -- update the list of derivations to choose from
    let updateDerTxt t = set derTxt [ text := "Deriviations (" ++ t ++ ")" ]
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
    return $ hfloatCentre $ container ib $ column 0 $
             [ row 5
                [ label "Show...", widget fullDervChk, widget srcTreeChk, widget detailsChk ]
             , row 5
                [ widget derTxt, widget derChoice
                , hspace 5, label "Node", widget jumpChoice, widget jumpBtn ]  ]
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


selectedDerivation :: CkyDebugParams -> CkyStatus -> CkyItem -> Tree (ChartId, String)
selectedDerivation f s c =
 let derivations = extractDerivations s c
     whichDer    = debugWhichDerivation f
 in if boundsCheck whichDer derivations
       then derivations !! whichDer
       else geniBug $ "Bounds check failed on derivations selector:\n"
                      ++ "Selected derivation: " ++ (show whichDer) ++ "\n"
                      ++ "Bounds: 0 to " ++ (show $ length derivations - 1)

derivationNodes :: Tree (ChartId, String) -> [ChartId]
derivationNodes = (map fst).flatten

-- | Remove na and subst/adj completion links
thinDerivationTree :: Tree (ChartId, String) -> Tree (ChartId, String)
thinDerivationTree =
 let thinlst = ["no-adj", "subst", "adj" ]
     helper n@(Node _ []) = n
     -- this is made complicated for fancy highlighting to work
     helper (Node (id,op) [k]) | op `elem` thinlst = (Node (id,op2) k2)
       where (Node (_,op2) k2) = helper k
     helper (Node x kids) = (Node x $ map helper kids)
 in  helper

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
       edgeParams (_, "kids"  ) = []
       edgeParams (_, "init"  ) = [ label "i" ]
       edgeParams (_, "subst" ) = [ substColor ]
       edgeParams (_, "adj"   ) = [ adjColor   ]
       edgeParams (_, "subst-finish") = [ substColor, style "bold"        , arrowtail "normal" ]
       edgeParams (_, "adj-finish")   = [ adjColor  , style "dashed, bold", arrowtail "normal" ]
       edgeParams (_, k) = [ ("label", "UNKNOWN: " ++ k) ]
       --
       whichDer    = debugWhichDerivation f
       showFullDer = debugShowFullDerv f
       showSrcTree = debugShowSourceTree f
       showTree i t = gvSubgraph $ gvShowTree edgeParams (s,showFullDer, [ciId c]) prfx t
                      where prfx = p ++ "t" ++ (show i)
       gvDerv = showTree whichDer $ if showFullDer then t else thinDerivationTree t
                where t = selectedDerivation f s c
       --
       joinedAut = uncurry mJoinAutomataUsingHole $ unpackItemToAuts s c
       gvAut     = graphvizShowAsSubgraph () (p ++ "aut")  joinedAut
       --
       showFeats  = debugShowFeats f
       treeParams = unlines $ graphvizParams showFeats $ ciSourceTree c
   -- FIXME: will have to make this configurable, maybe, show aut, show tree? radio button?
   in    "\n// ------------------- derivations --------------------------\n"
      ++ treeParams ++ "node [ shape = plaintext, peripheries = 0 ]\n"
      ++ gvDerv
      ++ "\n// ------------------- automata (joined) ------------------------\n"
      ++ gvSubgraph gvAut
      ++ if showSrcTree
         then ("\n// ------------------- elementary tree --------------------------\n"
               ++ treeParams ++ graphvizShowAsSubgraph f p c)
         else ""

instance GraphvizShowNode (CkyStatus,Bool,[ChartId]) (ChartId, String) where
  graphvizShowNode (st,showFullDerv,highlight) prefix (theId,_) =
   let idStr = show theId
       treename i = " (" ++ ((idname.ciSourceTree) i) ++ ")"
       txt = case findId st theId of
             Nothing   -> ("???" ++ idStr)
             Just item -> idStr ++ " " ++ (show.ciNode) item
                          ++ (if showFullDerv then treename item else "")
       custom = if theId `elem` highlight then [ ("fontcolor","red") ] else []
   in gvNode prefix txt custom

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
 te { ttree = ttree te
    , tsemantics  = bitVectorToSem (ciSemBitMap ci) (ciSemantics ci) }
 where te = ciSourceTree ci

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
      "node [ shape = ellipse, peripheries = 2 ]; "
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
