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

\chapter{Geni}
\label{cha:Geni}

Geni is the interface between the front and backends of the generator. The GUI
and the console interface both talk to this module, and in turn, this module
talks to the input file parsers and the surface realisation engine.  This
module also does lexical selection and anchoring because these processes might
involve some messy IO performance tricks.

\begin{code}
module Geni (ProgState(..), ProgStateRef, GeniInput(..), GeniResults(..), 
             showRealisations, groupAndCount,
             initGeni, runGeni, doGeneration, runMorph,
             loadGrammar, loadLexicon, 
             loadTestSuite, loadTargetSemStr,
             combine, testGeni)
where
\end{code}

\ignore{
\begin{code}
import Data.Char (toLower)
import qualified Data.Map as Map
import Data.IORef (IORef, readIORef, newIORef, modifyIORef)
import Data.List (intersperse, sort, nub, group, isPrefixOf)
import Data.Maybe (isNothing)
import Data.Tree

import System (ExitCode(ExitSuccess), 
               exitWith, getArgs)
import System.IO(hPutStrLn, hClose, hGetContents, hFlush, stdout)
import System.IO.Unsafe(unsafePerformIO)
import System.Mem
import Text.ParserCombinators.Parsec 
-- import System.Process 

import Control.Monad (when)
import CPUTime (getCPUTime)

import General(multiGroupByFM)

import Bfuncs (Macros, MTtree, ILexEntry, Lexicon, 
               Sem, SemInput,
               fromGConst, fromGVar, GeniVal(..),
               GNode, GType(Subs), Flist,
               isemantics, ifamname, iword, iparams, 
               ipfeat, ifilters,
               isempols, 
               gnname, gtype, gaconstr, gup, gdown, toKeys,
               sortSem, subsumeSem, params, 
               Subst, substSem, substFlist, substTree, substHelper,
               pidname, pfamily, pfeat, ptype, 
               ptpolarities, 
               setLexeme, tree, unifyFeat)

import Tags (Tags, TagElem, emptyTE, TagSite, 
             idname, tagLeaves,
             derivation, ttype, tsemantics, ttree, tsempols,
             tinterface, tpolarities, substnodes, adjnodes, 
             setTidnums, fixateTidnums)

import Configuration(Params, 
                     treatArgs, grammarType, tsFile,
                     testCases, morphCmd, ignoreSemantics,
                     selectCmd,
                     GrammarType(..),
                     macrosFile, lexiconFile, morphFile, rootCatsParam,
                     autopol, polarised, polsig, chartsharing, 
                     semfiltered, extrapol, footconstr)

import Mstate (Gstats, numcompar, szchart, geniter, initGstats,
               addGstats, initMState, runState, genstats,
               generate)

import Morphology
import Polarity
--import Predictors (PredictorMap, mapByPredictors, 
--                   fillPredictors, optimisePredictors)

import GeniParsers (parseMac, parseLex, cgmLexicon,
                    parseMorph, parseFil,
                    parseTSem, parseTSuite,
                    E(..))
import Debug.Trace
import Btypes (emptyLE)
--import Bfuncs (showSem,showPairs)
--showlex l = iword l ++ "\n sem: " ++ (showSem.isemantics) l ++ "\n enrich: " ++ (showPairs.ipfeat) l ++ "\n--\n" 

-- Not for windows
-- FIXME: even better would be to really figure out all this
-- Posix stuff so that I don't need to use my SysGeni hack
#ifndef mingw32_BUILD_OS 
import SysGeni 
#endif
\end{code}
}

\begin{code}
myEMPTY :: String
myEMPTY = "MYEMPTY" 

testGeni = True
\end{code}

% --------------------------------------------------------------------
\section{ProgState}
% --------------------------------------------------------------------

Data types for keeping track of the program state.  

\begin{description}
\item[pa] the current configuration being processed
\item[batchPa] the list of possible configurations.  This list is
               never empty.  If we are doing batch processing, 
               then its only item is pa
\item 
\end{description}

Note: if tags is non-empty, we can ignore gr and le

\begin{code}
data ProgState = ST{pa     :: Params,
                    --
                    gr       :: Macros,
                    le       :: Lexicon,
                    morphinf :: MorphFn,
                    ts       :: SemInput, 
                    -- names of test cases
                    tcases   :: [String], 
                    --name, sem, sentences
                    tsuite   :: [(String,SemInput,[String])] 
               }

type ProgStateRef = IORef ProgState

rootCats :: ProgState -> [String]
rootCats = (rootCatsParam . pa)
\end{code}

% --------------------------------------------------------------------
\section{Entry point}
% --------------------------------------------------------------------

This module mainly exports two monadic functions: an initialisation step
and a generation step.  In figure \ref{fig:code-outline-main} we show
what happens at the entry point: First \fnref{initGeni} is called to
start things off, then control is handed off to either the console or
the graphical user interface.  These functions then invoke the
generation step \fnref{runGeni} passing it one of \fnref{doGeneration}
or \fnref{debugGui}.

\begin{figure}
\begin{center}
\includegraphics[scale=0.5]{images/code-outline-main}
\label{fig:code-outline-main}
\caption{How the GenI entry point is used}
\end{center}
\end{figure}

\subsection{Initialisation step}
\label{fn:initGeni}

initGeni should be called when Geni is started.  This is typically
called from the user interface code.

\begin{code}
initGeni :: IO ProgStateRef 
initGeni = do
    args     <- getArgs
    confArgs <- treatArgs args
    -- Initialize the general state.  
    pstRef <- newIORef ST{pa = confArgs,
                          gr = [],
                          le = Map.empty,
                          morphinf = const Nothing,
                          ts = ([],[]),
                          tcases = [],
                          tsuite = [] }
    return pstRef 
\end{code}

\subsection{Generation step}

In the generation step, we first perform lexical selection, set up any
optimisations on the lexical choices, and then perform generation.

\paragraph{runGeni} \label{fn:runGeni} performs the entire Geni pipeline
: it does lexical selection, sets up any neccesary optimisations, runs
the generator proper, and returns the results with basic timing info.
We specify a \fnparam{runFn} argument, an IO monadic function which runs
the generator proper. This is used to run the generator with a debugger
as with \fnref{debugGui}, but for the most part, you want the vanilla
flavoured generator, \fnref{doGeneration}.

\begin{code}
runGeni :: ProgStateRef -> GeniFn -> IO GeniResults 
runGeni pstRef runFn = do 
  -- lexical selection
  mstLex   <- readIORef pstRef
  purecand <- runLexSelection mstLex
  -- strip morphological predicates
  let (tsem,tresLex) = ts mstLex
      tsemLex        = filter (isNothing.(morphinf mstLex)) tsem
  modifyIORef pstRef ( \x -> x{ts = (tsemLex,tresLex)} )
  pst      <- readIORef pstRef
  -- force the grammar to be read before the clock
  when (-1 == (length.show.le) pst) $ exitWith ExitSuccess
  when (-1 == (length.show.gr) pst) $ exitWith ExitSuccess
  performGC
  -- -- force lexical selection to be run before clock
  -- when (-1 == (length.show) purecand) $ exitWith ExitSuccess
  clockBefore <- getCPUTime 
  -- do any optimisations
  let config   = pa pst
      extraPol    = extrapol config  
      isPol       = polarised config
  let -- polarity optimisation (if enabled)
      preautcand = purecand
      autstuff   = buildAutomaton preautcand pst
      finalaut   = (snd.fst) autstuff
      lookupCand = lookupAndTweak (snd autstuff)
      pathsLite  = walkAutomaton finalaut 
      paths      = map (concatMap lookupCand) pathsLite 
      combosPol  = if isPol then paths else [preautcand]
      -- chart sharing optimisation (if enabled)
      isChartSharing = chartsharing config
      combosChart = if isChartSharing 
                    then [ detectPolPaths combosPol ] 
                    else map defaultPolPaths combosPol 
      -- 
      combos = map (fixateTidnums.setTidnums) combosChart
      fstGstats = initGstats
  -- do the generation
  let genifnInput = GI { giSem = tsem
                       , giCands = preautcand
                       , giAuts  = fst autstuff
                       , giTrees = combos }
  (res, gstats') <- runFn pst genifnInput 
  let gstats = addGstats fstGstats gstats'
  -- statistics 
  let statsOpt =  if (null optAll) then "none " else optAll
                  where polkey   = "pol"
                        polplus  =    (if autopol config then "A" else "")
                                   ++ (if polsig  config then "S" else "")
                                   ++ (if isChartSharing then "C" else "")
                        --
                        adjplus  =    (if semfiltered config then "S" else "")
                                   ++ "O" -- always uses ordered adjunction
                                   ++ (if footconstr  config then "F" else "")
                        --
                        optAll   = optPol ++ optAdj
                        optPol   = if isPol     
                                   then (if null polplus then polkey else polkey ++ ":" ++ polplus) 
                                   else "" 
                        optAdj   = if null adjplus then "" else " adj:" ++ adjplus
      statsAut     = if isPol then show $ length combosPol else ""
      ambiguityStr = show $ calculateTreeCombos preautcand 
  -- pack up the results
  let results = GR { -- grCand     = preautcand,
                     -- grAuts = auts,
                     -- grCombos   = combos,
                     grDerived  = res, 
                     grSentences = [],
                     grOptStr   = (statsOpt, showLitePm extraPol),
                     grAutPaths = statsAut,
                     grTimeStr  = "",
                     grAmbiguity = ambiguityStr,
                     grStats    = gstats }
  -- note: we have to do something with the results to force evaluation
  -- of the generator (for timing)
  when (length (show results) == 0) $ exitWith ExitSuccess
  clockAfter  <- getCPUTime 
  let timediff = (fromInteger $ clockAfter - clockBefore) / 1000000000
      statsTime = (show $ timediff) 
  when (length statsTime == 0) $ exitWith ExitSuccess
  -- morphology 
  let uninflected = map tagLeaves res
  sentences <- runMorph pstRef uninflected
  -- final results 
  return (results { grSentences = map (map toLower) sentences,
                    grTimeStr  = statsTime })
\end{code}

\paragraph{GeniFn and GeniInput} define the type for \fnparam{runFn}
argument in \fnreflite{runGeni}.  This function should take as input
not only the input semantics, but all the intermediary results from
\fnreflite{runGeni}, that is, the lexical selection, automata, etc.
The vanilla generator \fnref{doGeneration} ignores most of this  
intermediary stuff, but the debugger tries to display or do other 
interesting things with them.

\begin{code}
type GeniFn = ProgState -> GeniInput -> IO ([TagElem], Gstats)
data GeniInput = GI { giSem   :: Sem
                    , giCands :: [TagElem]
                    , giTrees :: [[TagElem]]
                    , giAuts  :: ([AutDebug], PolAut) }
\end{code}

% --------------------------------------------------------------------
\section{Lexical selection}
\label{sec:candidate_selection} \label{sec:lexical_selecetion} \label{par:lexSelection}
% --------------------------------------------------------------------

\paragraph{runLexSelection} \label{fn:runLexSelection} determines which
candidates trees which will be used to generate the current target semantics.  

\begin{code}
runLexSelection :: ProgState -> IO [TagElem]
runLexSelection pst = 
  case (grammarType $ pa pst) of  
        CGManifesto -> runCGMLexSelection pst
        _           -> runBasicLexSelection pst

runBasicLexSelection :: ProgState -> IO [TagElem]
runBasicLexSelection pst = do
  let (tsem,_) = ts pst
      lexicon  = le pst
      -- select lexical items first 
      lexCand   = chooseLexCand lexicon tsem
      -- then anchor these lexical items to trees
      combiner = combineList (gr pst) 
      cand     = concatMap combiner lexCand
      -- attach any morphological information to the candidates
      morphfn  = morphinf pst
      cand2    = attachMorph morphfn tsem cand 
  return $ setTidnums cand2
\end{code}

\paragraph{buildAutomaton} constructs the polarity automaton from the
lexical selection.

\begin{code}
buildAutomaton :: [TagElem] -> ProgState -> (PolResult, TagLite -> [TagElem])
type PolResult = ([(String, PolAut, PolAut)], PolAut)

buildAutomaton candRaw pst =
  let config   = pa pst
      (tsem,tres) = ts pst
      -- restrictors and extra polarities
      mergePol = Map.unionWith (+)
      rootCatPref = prefixRootCat $ head $ rootCats pst
      rcatPol = if (null $ rootCats pst)
                then Map.empty
                else Map.singleton rootCatPref (-1)
      extraPol = mergePol (extrapol config) $ mergePol rest rcatPol
                 where rest = declareRestrictors tres
      detect   = detectRestrictors tres
      restrict t = t { tpolarities = mergePol p r
                     } --, tinterface  = [] }
                   where p  = tpolarities t
                         r  = (detect . tinterface) t
      candRest  = map restrict candRaw 
      -- polarity detection (if needed)
      isAutoPol = autopol   config
      cand = if isAutoPol 
             then detectPols candRest
             else candRest
      -- building the automaton
      (candLite, lookupCand) = reduceTags (polsig config) cand
      auts = makePolAut candLite tsem extraPol 
  in (auts, lookupCand)
\end{code}

% --------------------------------------------------------------------
\subsection{Basic selection}
\subsubsection{Combine}
\label{sec:combine_macros}
% --------------------------------------------------------------------

combine: Given 
- the Macros and 
- a list of ILexEntry (read from the Lexicon.in file) 

it creates the Tags repository combining lexical entries and
un-anchored trees from the grammar. It also unifies the parameters
used to specialize un-anchored trees and propagates additional features
given in the ILexEntry. 

\begin{code}
combine :: Macros -> Lexicon -> Tags
\end{code}

We start by collecting all the features and parameters we want to combine.

\begin{code}
combine gram lexicon =
  let helper li = map (combineOne li) macs 
       where tn   = ifamname li
             macs = [ t | t <- gram, pfamily t == tn ]
  in Map.map (\e -> concatMap helper e) lexicon 
\end{code}

\paragraph{combineList} takes a lexical item; it looks up the tree
families for that item, and anchors the item to the trees.  A simple
list of trees is returned.

\begin{code}
combineList :: Macros -> ILexEntry -> [TagElem]
combineList gram lexitem = 
  let tn      = ifamname lexitem
      macs    = [ t | t <- gram, pfamily t == tn ]
      result  = map (combineOne lexitem) macs
      warning = "Warning: family " ++ tn ++ " not found in Macros"
  in unsafePerformIO $ do 
       when (null macs) $ putStrLn warning 
       return result 
\end{code}

\paragraph{combineOne} \label{fn:combineOne} combines a single tree with its
lexical item to form a bonafide TagElem

\begin{code}
combineOne :: ILexEntry -> MTtree -> TagElem
combineOne lexitem e = 
   let wt = "(Word: "++ (iword lexitem) ++
            ", Family:" ++ (ifamname lexitem) ++ ")\n"
       -- lexitem stuff
       sem  = isemantics lexitem
       p    = iparams lexitem
       pf   = ipfeat lexitem
       -- tree stuff
       tp   = map fromGVar $ params e
       tpf  = pfeat e
       -- unify the parameters
       psubst = zip tp p
       paramsUnified = substTree (Bfuncs.tree e) psubst 
       -- unify the features
       pf2  = substFlist pf  psubst
       tpf2 = substFlist tpf psubst
       (fsucc, funif, fsubst) = unifyFeat pf2 tpf2
       featsUnified = substTree paramsUnified fsubst 
       -- detect subst and adj nodes
       unified = featsUnified
       (snodes,anodes) = detectSites unified 
       -- the final result
       showid i = if null i then "" else ("-" ++ i)
       sol = emptyTE {
                idname = iword lexitem ++ "_" 
                         ++ pfamily e ++ showid (pidname e),
                derivation = (0,[]),
                ttype = ptype e,
                ttree = setLexeme (iword lexitem) unified,
                substnodes = snodes,
                adjnodes   = anodes,
                tsemantics = substSem sem fsubst,
                tpolarities = ptpolarities e,
                tsempols    = isempols lexitem,
                tinterface  = funif
                -- tpredictors = combinePredictors e lexitem
               }        
       -- well... with error checking
       wterror s = error (s ++ " " ++ wt)
       result 
         -- if the parameters are of different length
         | (length p) /= (length tp) = wterror "Wrong number of parameters."
{-
  This check prevents us from using large families of trees with
  very different interfaces

         -- if the lex item features are not a subset of the tree features
         | intersect fpf ftpf /= fpf = error $
             "Lex entry " ++ iword lexitem 
             ++ "'s features are not a subset of"
             ++ " tree's features: "  
             ++ "\nlex:  " ++ show fpf  
             ++ "\ntree: " ++ show ftpf
-}
         -- if fs unification fails for any reason (probably a bug)
         | not fsucc = wterror "Feature unification failed."
         -- success! 
         | otherwise = sol
   in result
\end{code}

\paragraph{detectSites}: Given a tree(GNode) 
returns a list of substitution or adjunction nodes

\begin{code}
detectSites :: Tree GNode -> ([TagSite], [TagSite])
detectSites (Node a lt) =
  let next = map detectSites lt
      (snodes', anodes') = unzip next
      --
      site = [(gnname a, gup a, gdown a)]
      snodes = if (gtype a == Subs) then (site:snodes') else snodes' 
      anodes = if (gaconstr a)      then anodes' else (site:anodes')
  in (concat snodes, concat anodes)
\end{code}

% --------------------------------------------------------------------
\subsubsection{The selection process}
% --------------------------------------------------------------------

\paragraph{chooseLexCand} selects and returns the set of entries from
the lexicon whose semantics subsumes the input semantics. 

\begin{code}
chooseLexCand :: Lexicon -> Sem -> [ILexEntry]
chooseLexCand slex tsem = 
  let -- the initial "MYEMPTY" takes care of items with empty semantics
      keys = myEMPTY:(toKeys tsem)   
      -- we choose candidates that match keys
      lookuplex t = Map.findWithDefault [] t slex
      cand    = concatMap lookuplex keys
      -- and refine the selection... 
  in chooseCandI tsem cand
\end{code}

With a helper function, we refine the candidate selection by
instatiating the semantics, at the same time filtering those which
do not stay within the target semantics, and finally eliminating 
the duplicates.

\begin{code}
chooseCandI :: Sem -> [ILexEntry] -> [ILexEntry]
chooseCandI tsem cand =
  let substLex i (sem,sub) = 
        i { isemantics = sem 
          , ipfeat     = substFlist (ipfeat i)   sub  
          , iparams    = substPar  (iparams i)   sub
          }
      --
      substPar :: [GeniVal] -> Subst -> [GeniVal]
      substPar par sub = map (\p -> substOne p sub) par
      substOne :: GeniVal -> Subst -> GeniVal
      substOne p sub = foldl (flip substHelper) p sub
      --
      helper :: ILexEntry -> [ILexEntry]
      helper le = if (null sem) then [le] 
                  else map (substLex le) psubsem 
        where psubsem = subsumeSem tsem sem
              sem = isemantics le
      --
  --in --trace ("\n\n" ++ (concatMap showlex cand)) $ 
  in nub $ concatMap helper cand 
\end{code}

\paragraph{mapBySemKeys} organises items by their semantic key.  A
semantic key is a semantic literal boiled down to predicate plus arity
(see section \ref{btypes_semantics}).  Given \texttt{xs} a list of items
and \texttt{fn} a function which retrieves the item's semantics, we
return a Map from semantic key to a list of items with that key.
An item may have multiple keys.

This is used to organise the lexicon by its semantics.

\begin{code}
mapBySemKeys :: (a -> Sem) -> [a] -> Map.Map String [a]
mapBySemKeys semfn xs = 
  let gfn t = if (null s) then [myEMPTY] else toKeys s 
              where s = semfn t
  in multiGroupByFM gfn xs
\end{code}

% --------------------------------------------------------------------
\subsection{CGM selection}
\label{sec:cgm_selection}
% --------------------------------------------------------------------

The common grammar manifesto \cite{kow05CGM} (CGM) is an internal LORIA
initiative to maintain a TAG grammar and lexicon that can be used both
for parsing and generation.  One feauture of the CGM is that tree
selection and anchoring are farmed out to a third party tool we call the
the Selector.  This module handles most of the CGM-specific bits of 
the lexical selection process via a CGM lexicon.  Note that we also have
some bits of code in Lparser.y for parsing said lexicons.

\paragraph{runCGMLexSelection} is the front end to the CGM lexical
selection process.  

\begin{code}
runCGMLexSelection :: ProgState -> IO [TagElem]
runCGMLexSelection pst = 
  do let (tsem,_) = ts pst
         lexicon  = le pst
     -- figure out what grammar file to use
     let gparams  = pa pst
         gramfile = macrosFile gparams
     -- select lexical items 
         lexCand = chooseLexCand lexicon tsem
         -- lexCand = trace ("\n===============\n" ++ (concatMap showlex lexCand')) $ lexCand'
     -- run the selector module
     let idxs = [1..]
         fil  = concat $ zipWith lexEntryToFil lexCand idxs 
     selected <- runSelector pst gramfile fil
     let g = case (parseMac selected) of 
              Ok x     -> x 
              Failed x -> error x 
         --
         lexMap = Map.fromList $ zip (map show idxs) lexCand
         fixate :: MTtree -> IO TagElem 
         fixate ts = 
           case (Map.lookup id lexMap) of
             Nothing  -> fail ("no such lexical entry " ++ id)
             Just lex -> return $ combineCGM lex ts 
           where id  = reverse $ tail $ dropWhile (/= 'x') $ reverse fam -- FIXME : HACK!
                 fam = tail $ pfamily ts -- FIXME: hack! 
     cand <- mapM fixate g
     -- attach any morphological information to the candidates
     let morphfn  = morphinf pst
         cand2    = attachMorph morphfn tsem cand 
     return $ setTidnums cand2
  `catch` \e -> do putStrLn (show e)
                   return [] 
\end{code}

\subsubsection{Calling the Selector}

\paragraph{runSelector} calls the Selector, passing it the
grammar file (argument \fnparam{gfile}) and the
lexical selection (argument \fnparam{l}).
It returns a list of anchored trees.

The selector is expected to read cgm filter stuff 
(see \cite{kow05CGM} and lexEntryToFil below) and output 
a set of geni formatted trees\footnote{What actually happens
is that it outputs a set of XML trees; and we use the XML
converter in \ref{cha:xml} to convert that to geni format}.

\begin{code}
runSelector :: ProgState -> String -> String -> IO String 
runSelector pst gfile fil = do
#ifdef mingw32_BUILD_OS
     putStr $ "Selector not available under Windows until Eric"
              ++ " figures out all this Posix stuff.\n"
     return ""
#else
      -- run the selector
     let theCmd  = selectCmd (pa pst)
         selectArgs = [gfile]
     when (null theCmd) $ fail "Please specify a tree selection command!"
     putStr $ "Selector started.\n"
     hFlush stdout
     -- (toP, fromP, _, pid) <- runInteractiveProcess selectCmd selectArgs Nothing Nothing
     (pid, fromP, toP) <- runPiped theCmd selectArgs Nothing Nothing
     hPutStrLn toP fil
     hClose toP -- so that process gets EOF and knows it can stop
     res    <- hGetContents fromP 
     exCode <- awaitProcess pid 
     {- case exCode of 
       Just ExitSuccess -> return ()
       _ -> fail "There was a problem running the selector. Check your terminal." -}
     return res
#endif
\end{code}

\paragraph{lexEntryToFil} converts a lexical entry to a CGM filter for
use by the selection module.  The selection module is a third-party
program which selects the trees from a grammar that correspond to each
lexical item and which performs enrichement and anchoring.  The
arguments are \fnparam{lex}, the lexical item to convert; and
\fnparam{n}, the numerical id you wish to associate to that item.  

Note: One weird thing we do in this function is to add some stuff to
the path equations for enrichment.  This stuff corresponds to semantic
arguments.  For example, if the semantics of the lexical item is
\semexpr{hate(m,j)}, we add the equations \verb!interface.arg1=m! and
\verb!interface.arg2=j!.  In order to do this, we assume that there is
only one literal in the lexical item semantics.

\begin{code}
lexEntryToFil :: ILexEntry -> Int -> String
lexEntryToFil lex n = 
  let filters   = ifilters lex
      enrichers = ipfeat lex 
      --
      showFil (a,v) = a ++ ":" ++ show v
      showEnr (a,v) = a ++ "=" ++ show v
      concatSperse x y = concat $ intersperse x y
  in show n 
    ++ " " ++ iword lex ++ " "
    ++ "[" 
    ++ (concatSperse ","   $ map showFil filters)
    ++ "]\n(" 
    ++ (concatSperse ",\n" $ map showEnr enrichers)
    ++ ")\n\n"
\end{code}

\paragraph{combineCGM} is similar to \fnref{combineOne} except that we assume
the tree is completely anchored and instatiated and that thus there no boring
unification or checks to worry about.

\begin{code}
combineCGM :: ILexEntry -> MTtree -> TagElem
combineCGM lexitem e = 
   let tree_ = Bfuncs.tree e
       (snodes,anodes) = detectSites tree_
       -- FIXME: dirty hack strips off the semantic handle
       -- the final result
       sol = emptyTE {
                idname = iword lexitem ++ "_" ++ pidname e,
                derivation = (0,[]),
                ttype  = ptype e,
                ttree  = tree_,
                substnodes = snodes,
                adjnodes   = anodes,
                -- FIXME: we completely ignore semantic info from the tree
                -- this allows us to handle multi-literal semantics
                tsemantics = isemantics lexitem,
                tpolarities = Map.empty,
                tsempols    = isempols lexitem,
                tinterface  = pfeat e
                -- tpredictors = combinePredictors e lexitem
               }        
   in sol 
\end{code}

% --------------------------------------------------------------------
\section{Loading and parsing}
% --------------------------------------------------------------------

\subsection{Grammars}

Grammars consist of the following:
\begin{enumerate}
\item index file - which tells where the other files
      in the grammar are (relative to the grammar)
\item semantic lexicon file - semantics $\rightarrow$ lemma
\item lexicon file - lemma $\rightarrow$ families
\item macros file  - unlexicalised trees
\end{enumerate}

The generator reads these into memory and combines them into a grammar
(page \pageref{sec:combine_macros}).

\paragraph{loadGrammar} \label{fn:loadGrammar} Given the pointer to the
monadic state pstRef it reads and parses the grammar file index; and
from this information, it reads the rest of the grammar (macros,
lexicon, etc).  The Macros and the Lexicon 

\begin{code}
loadGrammar :: ProgStateRef -> IO() 
loadGrammar pstRef =
  do pst <- readIORef pstRef
     --
     let config    = pa pst
         gfilename = macrosFile config 
         lfilename = lexiconFile config 
         sfilename = tsFile config
     -- display 
     let errorlst  = filter (not.null) $ map errfn src 
           where errfn (err,msg) = if err then msg else ""
                 src = [ (null gfilename, "a tree file")
                       , (null lfilename, "a lexicon file") 
                       , (null sfilename, "a test suite") ]
         errormsg = "Please specify: " ++ (concat $ intersperse ", " errorlst)
     when (not $ null errorlst) $ fail errormsg 
     -- we don't have to read in grammars from the simple format
     case grammarType config of 
        CGManifesto -> return ()
        _           -> loadGeniMacros pstRef config
     -- in any case, we have to...
     loadLexicon   pstRef config 
     loadMorphInfo pstRef config 
     loadTestSuite pstRef 
\end{code}

\subsubsection{Lexicon}

\paragraph{loadLexicon} Given the pointer to the monadic state pstRef and
the parameters from a grammar index file parameters; it reads and parses
the lexicon file and the semantic lexicon.   These are then stored in
the monad.

FIXME: differentiate
\begin{code}
loadLexicon :: ProgStateRef -> Params -> IO ()
loadLexicon pstRef config =
  case (grammarType config) of 
        CGManifesto -> loadCGMLexicon  pstRef config 
        _           -> loadGeniLexicon pstRef config 
\end{code}

\paragraph{loadGeniLexicon} Given the pointer to the monadic state pstRef and
the parameters from a grammar index file parameters; it reads and parses
the lexicon file and the semantic lexicon.   These are then stored in
the mondad.

\begin{code}
loadGeniLexicon :: ProgStateRef -> Params -> IO ()
loadGeniLexicon pstRef config = do 
       let lfilename = lexiconFile config
       when (null lfilename) $ fail "Please specify a lexicon!"
       putStr $ "Loading Lexicon " ++ lfilename ++ "..."
       hFlush stdout
       lf <- readFile lfilename 
       pst <- readIORef pstRef
       let params = pa pst
           --       
           getSem l  = if (ignoreSemantics params) then [] else isemantics l 
           sorter l  = l { isemantics = (sortSem . getSem) l }
           cleanup   = (mapBySemKeys isemantics) . (map sorter)
           --
           lex = case parseLex lf of 
                   Ok x     -> cleanup x 
                   Failed x -> error x 
       --
       putStr ((show $ length $ Map.keys lex) ++ " entries\n")
       -- combine the two lexicons
       {- trace (concatMap (concatMap showlex) $ eltsFM lex) $ -}
       modifyIORef pstRef (\x -> x{le = lex})

       return ()
\end{code}

%\paragraph{setPrecedence} takes two lists of precedence directives
%(FIXME: explained where?) and a lemma lexicon.  It returns the lemma
%lexicon modified so that each item is assigned a precedence according to
%its membership in the list of precedence directives.  The first list of
%directives are items with tight precedence; the second list are those
%with loose precedence.  In both lists, items closest to the front have
%tightest precedence.  Items which are not in either the tight or loose
%precedence list have default precedence.
%
%\begin{code}
%
%
%setPrecedence :: [[WordCat]] -> [[WordCat]] 
%                 -> LemmaLexicon -> LemmaLexicon
%setPrecedence tight loose lemlex =
%  let start  = 0 - (length tight)
%      tightp = zip [start..(-1)] tight
%      loosep = zip [0..] loose
%      --
%      tightlex = foldr setPrecedence' lemlex   tightp
%  in foldr setPrecedence' tightlex loosep
%
%setPrecedence' :: (Int,[WordCat]) -> LemmaLexicon -> LemmaLexicon
%setPrecedence' (pr,items) lemlex =
%  let setpr li     = li {iprecedence = pr}
%      --
%      helper :: WordCat -> LemmaLexicon -> LemmaLexicon
%      helper wc ll = addToFM ll wc (map setpr lems)
%                     where lems = lookupWithDefaultFM ll [] wc 
%  in foldr helper lemlex items
%\end{code}

\subsubsection{Lexicon - CGM}

\paragraph{loadCGMLexicon} Given the pointer to the monadic state pstRef and
the parameters from a grammar index file parameters; it reads and parses
the lexicon file using the common grammar manifesto format.  

\begin{code}
loadCGMLexicon :: ProgStateRef -> Params -> IO ()
loadCGMLexicon pstRef config = do 
       let lfilename = lexiconFile config
       when (null lfilename) $ fail "Please specify a lexicon!"
       putStr $ "Loading CGManifesto Lexicon " ++ lfilename ++ "..."
       hFlush stdout
       parsed <- parseFromFile cgmLexicon lfilename
       case parsed of 
         Left  err     -> fail (show err)
         Right entries -> let lexicon = mapBySemKeys isemantics entries
                          in  modifyIORef pstRef (\x -> x{le = lexicon})
                          
\end{code}

\subsubsection{Macros}

\paragraph{loadGeniMacros} Given the pointer to the monadic state pstRef and
the parameters from a grammar index file parameters; it reads and parses
macros file.  The macros are stored as a hashing function in the monad.

\begin{code}
loadGeniMacros :: ProgStateRef -> Params -> IO ()
loadGeniMacros pstRef config = 
  do let filename = macrosFile config
     --
     putStr $ "Loading Macros " ++ filename ++ "..."
     hFlush stdout
     gf <- readFile filename
     when (null gf) $ fail "Please specify a trees file!"
     let g = case (parseMac gf) of 
                   Ok x     -> x 
                   Failed x -> fail x 
         sizeg  = length g
     putStr $ show sizeg ++ " trees in " 
     putStr $ (show $ length g) ++ " families\n"
     modifyIORef pstRef (\x -> x{gr = g})
\end{code}

\subsubsection{Misc}

\paragraph{loadMorphInfo} Given the pointer to the monadic state pstRef and
the parameters from a grammar index file parameters; it reads and parses
the morphological information file, if available.  The results are stored
as a lookup function in the monad.

\begin{code}
loadMorphInfo :: ProgStateRef -> Params -> IO ()
loadMorphInfo pstRef config = 
  do let filename = morphFile config
     when (not $ null filename ) $ do --
        putStr $ "Loading Morphological Info " ++ filename ++ "..."
        hFlush stdout
        gf <- readFile filename
        let g = case parseMorph gf of
                  Ok x     -> x
                  Failed x -> fail x
            sizeg  = length g
        putStr $ show sizeg ++ " entries\n" 
        modifyIORef pstRef (\x -> x{morphinf = readMorph g})
\end{code}

\subsection{Target semantics}

\paragraph{loadTestSuite} \label{fn:loadTestSuite} 
given a pointer pstRef to the general state st, it access the parameters and the
name of the file for the target semantics from params.  It parses the file as a
test suite, and assigns it to the tsuite field of st.

\begin{code}
loadTestSuite :: ProgStateRef -> IO ()
loadTestSuite pstRef = do
  pst <- readIORef pstRef
  let config   = pa pst
      filename = (tsFile.pa) pst
      useSem   = not $ ignoreSemantics config
  when (null filename) $ fail "Please specify a test suite!"
  when useSem $ do
    putStr $ "Loading Test Suite " ++ filename ++ "...\n"
    hFlush stdout
    tstr <- readFile filename
    -- helper functions for test suite stuff
    let cleanup (id, (sm,sr), sn) = (id, newsmsr, sort sn)
          where newsmsr = (sortSem sm, sort sr)
        updateTsuite s x = x { tsuite = map cleanup s   
                             , tcases = testCases config}
    let sem = parseTSuite tstr
    case sem of 
      Ok s     -> modifyIORef pstRef $ updateTsuite s 
      Failed s -> fail s
    -- in the end we just say we're done
    --putStr "done\n"
\end{code}

\paragraph{loadTargetSemStr} Given a string with some semantics, it
parses the string and assigns the assigns the target semantics to the 
ts field of the ProgState 

\begin{code}
loadTargetSemStr :: ProgStateRef -> String -> IO ()
loadTargetSemStr pstRef str = 
    do pst <- readIORef pstRef
       putStr "Parsing Target Semantics..."
       let semi = parseTSem str
           smooth (s,r) = (sortSem s, sort r)
       let params = pa pst
       if (ignoreSemantics params) then return ()  --modifyIORef pstRef (\x -> x{ts = ([],[])})
           else case semi of 
              Ok sr    -> modifyIORef pstRef (\x -> x{ts = smooth sr})
              Failed s -> fail s
       putStr "done\n"
\end{code}

%\paragraph{flattenTargetSem} takes a recursively embedded target
%semantics like \verb$love(me wear(you sw))$ \verb$sweater(sw)$
%and converts it into a flat semantics with handles like
%\verb$love(h1 me h1.3)$ \verb$wear(h1.3 you)$ $sweater(h2 sw)$
%
%FIXME: we do not know how to handle literals with no arguments!
%
%\begin{code}
%flattenTargetSem :: [Tree (String, String)] -> Sem
%flattenTargetSem trees = sortSem results
%  where results  = (concat . snd . unzip) results'
%        results' = zipWith fn [1..] trees 
%        fn g k   = flattenTargetSem' [g] k
%\end{code}
%
%\paragraph{flattenTargetSem'} We walk the semantic tree, returning an index and
%a list of predicates representing the flat semantics for the entire semantic
%tree.  Normally, only the second result is interesting for you; the first
%result is used for recursion, the idea being that a node's parameters is the
%list of indices returned by its children.  If a child does not have any
%children of its own, then its index is its string value (like \verb$john$); if
%it does have children, then we return a handle for it (like \verb$h1.3.4$)
%
%\begin{code}
%flattenTargetSem' :: [Int] -> Tree (String,String) -> (String, Sem)
%flattenTargetSem' _  (Node ("",pred) []) = (pred, []) 
%
%flattenTargetSem' gorn (Node (hand,pred) kids) =
%  let smooshGorn = "gh" ++ (concat $ intersperse "." $ map show gorn)
%      -- recursive step
%      kidGorn   = map (\x -> (x:gorn)) [1..] 
%      next      = zip kidGorn kids
%      nextRes   = map (\ (g,k) -> flattenTargetSem' g k) next
%      (kidIndexes, kidSem) = unzip nextRes
%      -- create the predicate
%      handle    = if null hand then smooshGorn else hand
%      result    = (handle, pred, kidIndexes)
%  in (smooshGorn, result:(concat kidSem))
%\end{code}

% --------------------------------------------------------------------
\section{Generation step} 
\label{fn:doGeneration}
% --------------------------------------------------------------------

Actually running the generator...  Note: the only reason this is monadic
is to be compatible with the debugger GUI.  There could be some code
simplifications in order.

\begin{code}
doGeneration :: GeniFn 
doGeneration pst input = do
  return (doGeneration' (pa pst) input) 

doGeneration' :: Params -> GeniInput -> ([TagElem], Gstats)
doGeneration' config input = 
  let tsem   = giSem   input
      combos = giTrees input
      -- do the generation
      genfn c = (res, genstats st) 
                where ist = initMState c [] tsem config
                      (res,st) = runState generate ist
      res' = map genfn combos
      addres (r,s) (r2,s2) = (r ++ r2, addGstats s s2)
  in foldr addres ([],initGstats) res'
\end{code}

\subsection{Returning results}

We provide a data structure to be used by verboseGeni for returning the results
(grDerived) along with the intermediary steps and some useful statistics.  

\begin{code}
data GeniResults = GR {
  -- optimisations and extra polarities
  grOptStr   :: (String,String),
  -- some numbers (in string form)
  grStats     :: Gstats,
  grAutPaths  :: String,
  grAmbiguity :: String,
  grTimeStr   :: String,
  -- the final results
  grDerived   :: [TagElem],
  grSentences :: [String]
} 
\end{code}

We provide a default means of displaying the results/u

\begin{code}
instance Show GeniResults where
  show gres = 
    let gstats = grStats gres 
        gopts  = grOptStr gres
        sentences = grSentences gres
    in    "Optimisations: " ++ fst gopts ++ snd gopts ++ "\n"
       ++ "\nAutomaton paths explored: " ++ (grAutPaths  gres)
       ++ "\nEst. lexical ambiguity:   " ++ (grAmbiguity gres)
       ++ "\nTotal agenda size: " ++ (show $ geniter gstats) 
       ++ "\nTotal chart size:  " ++ (show $ szchart gstats) 
       ++ "\nComparisons made:  " ++ (show $ numcompar gstats)
       ++ "\nGeneration time:  " ++ (grTimeStr gres) ++ " ms"
       ++ "\n\nRealisations:\n" ++ (showRealisations sentences)
\end{code}

\paragraph{groupAndCount} is a generic list-processing function.
It converts a list of items into a list of tuples (a,b) where 
a is an item in the list and b is the number of times a in occurs 
in the list.

\begin{code}
groupAndCount :: (Eq a, Ord a) => [a] -> [(a, Int)]
groupAndCount xs = 
  map (\x -> (head x, length x)) grouped
  where grouped = (group.sort) xs
\end{code}

\paragraph{showRealisations} shows the sentences produced by the
generator in a relatively compact form 

\begin{code}
showRealisations :: [String] -> String
showRealisations sentences =
  let sentencesGrouped = map (\ (s,c) -> s ++ count c) g
                         where g = groupAndCount sentences 
      count c = if (c > 1) 
                then " (" ++ show c ++ " instances)"
                else ""
  in if (null sentences)
     then "(none)"
     else concat $ intersperse "\n" $ sentencesGrouped
\end{code}

\paragraph{runMorph} inflects a list of sentences if a morphlogical generator
has been specified.  If not, it returns the sentences as lemmas.

\begin{code}
runMorph :: ProgStateRef -> [[(String,Flist)]] -> IO [String]
runMorph pstRef sentences = 
  do pst <- readIORef pstRef
     let mcmd = morphCmd (pa pst)
     if null mcmd
        then return (map sansMorph sentences)
        else inflectSentences mcmd sentences 
\end{code}
