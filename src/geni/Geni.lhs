\chapter{Geni.lhs}

\begin{code}
module Geni (State(..), PState, GeniResults(..), 
             showRealisations, showOptResults,
             verboseGeni, customGeni,
             initGeni, runGeni, 
             loadMacros, loadLexicon, loadGrammarXml, loadTargetSem,
             loadTargetSemStr,
             -- for debugging only
             combine, chooseCand)
where

import Data.Array (listArray, (!)) 
import Data.List (intersect, intersperse, sort, nub, group)
import Data.Tree
import IOExts(IORef, readIORef, newIORef, modifyIORef)

import System (ExitCode(ExitSuccess), 
               exitWith, getArgs)
 
import FiniteMap
import Monad (when)
import CPUTime (getCPUTime)
\end{code}

\begin{code}
import Btypes (Grammar, ILexEntry, Sem, Flist, 
               GNode, GType(Subs), 
               isemantics, itreename, iword, iparams, ipfeat,
               gnname, gtype, gaconstr, gup, gdown, toKeys,
               sortSem, subsumeSem, params, appendToVars,
               substFlist',
               pfeat, ptype, 
               ptpolarities, 
               setLexeme, tree)

import Tags (Tags, TagElem, emptyTE, TagSite, 
             idname, tidnum,
             derivation, ttype, tsemantics, ttree, 
             tpolarities, 
             substnodes, adjnodes, addToTags, findInTags, substTagElem)

import Configuration(Params, defaultParams, getConf, treatArgs,
                     macrosFile, lexiconFile, grammarXmlFile, tsFile, 
                     polarised, chartsharing, semfiltered, 
                     orderedadj, extrapol)

import Mstate (Gstats, numcompar, szchart, geniter, initGstats,
               addGstats, initMState, runState, genstats,
               generate)

import Polarity
import Treeprint (showLeaves)
--import Predictors (PredictorMap, mapByPredictors, 
--                   fillPredictors, optimisePredictors)
\end{code}

\begin{code}
import Lex2 (lexer)
import Mparser (mParser)
import Lparser (lParser)
import Tsparser (tsParser, E(..))
import GrammarXml (parseXmlGrammar)
\end{code}

% --------------------------------------------------------------------
\section{State}
% --------------------------------------------------------------------

Data types for keeping track of the program state.  

\begin{description}
\item[pa] the current configuration being processed
\item[batchPa] the list of possible configurations.  This list is
               never empty.  If we are doing batch processing, 
               then its only item is pa
\end{description}

Note: if tags is non-empty, we can ignore gr and le

\begin{code}
data State = ST{pa      :: Params,
                batchPa :: [Params], -- list of configurations
                tags    :: Tags,
                gr      :: Grammar,
                le      :: [ILexEntry],
                ts      :: Sem
               }

type PState = IORef State
\end{code}

% --------------------------------------------------------------------
\section{Entry point}
% --------------------------------------------------------------------

Geni uses three distinct steps

\begin{enumerate}
\item initialisation    - reads input grammar files and target semantics
\item lexical selection - chooses the trees which will be used for 
                          generation
\item generation        - performs the actual generation
\end{enumerate}

The first step can be found in the user interface code.  The
second and third step are bundled into a function that returns not
only the generated trees, but some intermediary steps and useful
statistics.

\begin{code}
verboseGeni :: PState -> IO GeniResults
verboseGeni pst = customGeni pst runGeni 
\end{code}

\paragraph{customGeni} lets you specify what function you want to use for
generation: this is useful because it lets you pass in a debugger
instead of the vanilla generator

\begin{code}
type GeniFn = Params -> Sem -> [[TagElem]] -> IO ([TagElem], Gstats)

customGeni :: PState -> GeniFn -> IO GeniResults 
customGeni pst runFn = do 
  mst         <- readIORef pst
  -- lexical selection
  cand <- runLexSelection pst
  -- force lexical selection (and hence grammar reading)
  -- to be evaluated before the clock 
  when (length (show cand) == -1) $ exitWith ExitSuccess
  clockBefore <- getCPUTime 
  -- do any optimisations
  let config = pa mst
      tsem  = ts mst
      extraPol = extrapol config 
  let -- map between TagLite and TagElem (for polarity optimisation)
      candLite     = map toTagLite cand
      fromTagLite  = listArray (1,length cand) cand
      lookupCand t = fromTagLite ! (fromInteger $ tlIdnum t)
  let -- polarity optimisation (if enabled)
      isPol = polarised config 
      -- (auts, finalaut) = debugMakePolAut candLite tsem extraPol
      finalaut         = makePolAut candLite tsem extraPol
      pathsLite        = walkAutomaton finalaut 
      paths            = map (map lookupCand) pathsLite 
      combosPol        = if isPol then paths else [cand]
      -- chart sharing optimisation (if enabled)
      isChartSharing = chartsharing config
      combosChart = if isChartSharing 
                    then [ detectPolPaths combosPol ] 
                    else map defaultPolPaths combosPol 
      -- predictors optimisation (if enabled) 
      {- FIXME: disabled until we get around to making predictors
         and chart sharing interact properly 
      isPredicting = predicting config 
      predictmap   = mapByPredictors cand
      notnullsem t = not.null $ tsemantics t
      candwithsem    = filter notnullsem cand
      optimisedPred  = optimisePredictors p predictmap
                       where p = if isPol then combosChart else [candwithsem]
      (combos,fstGstats) = if isPredicting  
                           then optimisedPred
                           else (fillPredictors combosChart predictmap, initGstats)
      -}
      -- 
      combos    = combosChart
      fstGstats = initGstats
  -- do the generation
  (res, gstats') <- runFn config tsem combos
  let gstats  = addGstats fstGstats gstats'
  -- statistics 
  let statsOpt =  if (null optAll) then "none " else optAll
                  where optAll   = optSem ++ optPol ++ optChart 
                                   ++ optOAdj
                        optPol   = if isPol then "pol " else ""
                        optChart = if isChartSharing then "c-shr " else ""
                        optSem   = if semfiltered config then "sfilt " else ""
                        optOAdj  = if orderedadj config then "oadj " else ""

      statsAut = if isPol 
                 then    (show $ length combosPol) ++ "/"
                      ++ (show $ calculateTreeCombos candLite) 
                 else ""
  -- pack up the results
  let results = GR { grCand = cand,
                     -- grAuts = auts,
                     grCombos   = combos,
                     grFinalAut = finalaut,
                     grDerived  = res, 
                     grOptStr   = (statsOpt, showLitePm extraPol),
                     grAutPaths = statsAut,
                     grTimeStr  = "",
                     grStats    = gstats }
  -- note: we have to do something with the results to force evaluation
  -- of the generator (for timing)
  when (length (show results) == 0) $ exitWith ExitSuccess
  clockAfter  <- getCPUTime 
  let timediff = (fromInteger $ clockAfter - clockBefore) / 1000000000
      statsTime = (show $ timediff) 
  when (length statsTime == 0) $ exitWith ExitSuccess
  -- one last addendum to the results 
  return (results { grTimeStr  = statsTime })
\end{code}

\subsection{Individual steps}

initGeni should be called when Geni is started.  This is typically
called from the user interface code.

\begin{code}
initGeni :: IO PState 
initGeni = do
    confGenirc <- getConf defaultParams
    args       <- getArgs
    let confArgs = treatArgs confGenirc args
    -- Initialize the general state.  
    pst <- newIORef ST{pa = head confArgs,
                       batchPa = confArgs, 
                       tags = emptyFM,
                       gr   = emptyFM,
                       le = [],
                       ts = []}
    return pst 
\end{code}

\paragraph{runLexSelection} determines which candidates trees which will
be used to generate the current target semantics.  

Notes: if there is an XML grammar, we ignore the macros and lexicon.
Also, we also assign a tree id to each selected tree, and we append 
some unique suffix (coincidentally the tree id) to each variable in
each selected tree. This is to avoid nasty collisions during 
unification as mentioned in section \ref{sec:fs_unification}).
\label{par:lexSelection}.

\begin{code}
runLexSelection :: IORef State -> IO [TagElem] 
runLexSelection pst = do
    mst <- readIORef pst
    let tsem     = ts mst
        preCombined = tags mst
        combined = if (isEmptyFM preCombined) 
                   then combine (gr mst) (le mst)
                   else preCombined
        cand = chooseCand combined tsem 
        --
        setnum c i = c { tidnum = i,
                         ttree  = appendToVars (mksuf i) (ttree c) }
        mksuf i = "-" ++ (show i)
    return $ zipWith setnum cand [1..]
\end{code}

\paragraph{runGeni} Actually running the generator...  Note: the only
reason this is monadic is to be compatible with the debugger GUI.  There
could be some code simplifications in order.

\begin{code}
runGeni :: GeniFn 
runGeni config tsem combos = do
  return (runGeni' config tsem combos) 

runGeni' :: Params -> Sem -> [[TagElem]] -> ([TagElem], Gstats)
runGeni' config tsem combos = 
  -- do the generation
  let genfn c = (res, genstats st) 
                where ist = initMState c [] tsem config
                      (res,st) = runState generate ist
      res' = map genfn combos
      addres (r,s) (r2,s2) = (r ++ r2, addGstats s s2)
  in foldr addres ([],initGstats) res'
\end{code}

% --------------------------------------------------------------------
\section{Combine}
\label{sec:combine_macros}
% --------------------------------------------------------------------

combine: Given 
- the Grammar and 
- a list of ILexEntry (read from the Lexicon.in file) 

it creates the Tags repository combining lexical entries and
un-anchored trees from the grammar. It also unifies the parameters
used to specialize un-anchored trees and propagates additional features
given in the ILexEntry. 

\begin{code}
combine :: Grammar -> [ILexEntry] -> Tags
combine _ [] = emptyFM
\end{code}

We start by collecting all the features and parameters we want to combine.

\begin{code}
combine g (lexitem:rest) =
   let trest = combine g rest -- rest of the combination
       --
       sem = isemantics lexitem
       tn = itreename lexitem
       wt = "(Word: "++ (iword lexitem) ++ ", Tree:" ++ (itreename lexitem) ++ ")\n"
       e  = case (lookupFM g tn) of
            Just tt -> tt
            Nothing -> error ("Tree used in lexicon not found in Grammar" ++ wt)
       p = iparams lexitem
       pf = ipfeat lexitem
       fpf = map fst pf
       tp = params e
       tpf = pfeat e
       ftpf = map fst tpf
\end{code}

Here we unify Features and Parameters.

\begin{code}
       paramsUnified = replacePar (zip tp p) (Btypes.tree e)
       (unified,snodes,anodes) = replaceFeat pf paramsUnified 
       --
       sol = emptyTE {
                idname = tn ++ "-" ++ (iword lexitem),
                derivation = (0,[]),
                ttype = ptype e,
                ttree = setLexeme (iword lexitem) unified,
                substnodes = snodes,
                adjnodes   = anodes,
                tsemantics = sem,
                tpolarities = ptpolarities e
                -- tpredictors = combinePredictors e lexitem
               }        
       keys = if null sem
              then ["MYEMPTY"]     -- words with empty semantics are add under key "MYEMPTY"
              else toKeys sem
\end{code}

Now we actually perform the process that we defined above.

\begin{code}
       in if ((length p) /= (length tp))  -- if the parameters are of different length
          then error ("Wrong number of parameters. " ++ wt)
          else if (intersect fpf ftpf /= fpf) -- if the features specified in ILexEntry 
                                              -- are not a subset of the ones 
                                              -- specified in the grammar.
               then error ("Feature atributes don't match. " ++ wt)
               else foldr (\k -> \fm -> addToTags fm k sol) trest keys
\end{code}

% --------------------------------------------------------------------
\subsection{Replace feat}
% --------------------------------------------------------------------

\paragraph{replaceFeat}: Given 
- a tree(GNode) and 
- a list of pairs (feature:value)
, replaces through the tree any appearance of (feature:whatever) by
(feature:value) and returns a list of substitution or adjunction nodes

\begin{code}
replaceFeat :: Flist -> Tree GNode -> (Tree GNode, [TagSite], [TagSite])
replaceFeat l (Node a lt) =
  let newa = updateNode2 l a
      next = map (replaceFeat l) lt
      (newlt, snodes', anodes') = unzip3 next
      --
      site = [(gnname newa, gup newa, gdown newa)]
      snodes = if (gtype newa == Subs)  then (site:snodes') else snodes' 
      anodes = if (gaconstr newa)       then anodes' else (site:anodes')
  in (Node newa newlt, concat snodes, concat anodes)
\end{code}

\begin{code}
updateNode2 :: Flist -> GNode -> GNode
updateNode2 [] a = a

updateNode2 ((at,v):l) a =
    let rn = updateNode2 l a
        -- Note that this isn't the same thing as substList'.
        -- Here we hunt the tree for attributes that appear 
        -- in our FeatList and update their values 
        rep (at',v') = (at', if (at' == at) then v else v')
        ngup = map rep (gup rn)
        ngdown = map rep (gdown rn)
        in rn{gup = ngup,                
              gdown = ngdown}
\end{code}

% --------------------------------------------------------------------
\subsection{Instatiation of arguments}
% --------------------------------------------------------------------

replacePar: Given 
   - a tree of (GNode) and 
   - a list of pairs of strings 

replaces through the tree the first component by the second component

\begin{code}
replacePar :: [(String,String)] -> Tree GNode -> Tree GNode
replacePar l (Node a []) = Node (updateNode1 l a) []

replacePar l (Node a lt) = 
    let newa = updateNode1 l a
        newlt = map (replacePar l) lt
        in Node newa newlt 
\end{code}

\begin{code}
updateNode1 :: [(String,String)] -> GNode -> GNode
updateNode1 [] a = a

updateNode1 ((x,y):l) a = 
    let rn     = updateNode1 l a
        rep f  = substFlist' f (x,y) 
        ngup   = rep (gup rn)
        ngdown = rep (gdown rn)
        in rn{gup = ngup,
              gdown = ngdown}
\end{code}

% --------------------------------------------------------------------
\section{Candidate selection}
% --------------------------------------------------------------------

chooseCand: It access the Grammar for the candidate tags and loads
them in the Agenda.  It checks for semantics which adds more than
the required input, discarding those tags.  It also instantiates the
semantics of candidates to match the target semantics. 

See page \pageref{fn:subsumeSem} for the definition of semantic subsumption.

\begin{code}
chooseCand :: Tags -> Sem -> [TagElem]
chooseCand t tsem =
    let keys = "MYEMPTY":(toKeys tsem)   -- the initial "" takes care of words with empty semantics
        -- we choose candidates that match keys
        cand = concatMap (findInTags t) keys
        -- we filter those that do not stay within the target semantics and instantiate 
        -- semantics and tree, finaly we eliminate duplicates.
        res = nub (concatMap (chooseCandI tsem) cand)
        in res

chooseCandI :: Sem -> TagElem -> [TagElem]
chooseCandI tsem te =
    let sem = tsemantics te
        psubst = if (null sem) then [[]] else subsumeSem tsem sem
    in map (substTagElem te) psubst 
\end{code}

% --------------------------------------------------------------------
\section{Loading and parsing}
% --------------------------------------------------------------------

\subsection{Grammars}

There are two types of GenI grammars.  

Hand-written grammars have a simple human-friendly format.  They consist
of a macros file and lexicon file.  The generator reads these into
memory and combines them into a grammar (page \pageref{sec:combine_macros}).

Automatically generated grammars are built from a meta-grammar compiler.
They are available as a single XML file containing the whole grammar 
(no need for combining).

\subsubsection{Hand-written grammars}

\paragraph{loadMacros} Given the pointer to the monadic state pst it
reads and parses the macros from the macro file indicated in the monad.
The Macros are storded as a hashing function in the monad.

\begin{code}
loadMacros :: PState -> IO ()
loadMacros pst =  
    do st <- readIORef pst
       let config   = pa st
           filename = macrosFile config 
       putStr $ "Loading Macros " ++ filename ++ "... "
       gf <- readFile filename
       let (g, u) = mParser (lexer gf)
           errmsg  = "Some trees in grammar declared neither initial nor auxiliar.\n  Aborting.\n"
       if not (null u)
          then error (errmsg ++ (show u))
          else do putStr ((show $ sizeFM g) ++ " trees\n")
                  modifyIORef pst (\x -> x{gr = g})
                  return ()
\end{code}

\paragraph{loadLexicon} Given the pointer to the monadic state pst, it
reads and parses the lexicon from the lexicon file indicated in the
monad.  The lexicon is stored as a list in the monad.

\begin{code}
loadLexicon :: PState -> IO ()
loadLexicon pst = do 
       st <- readIORef pst
       let config = pa st
           filename = lexiconFile config
       putStr $ "Loading Lexicon " ++ filename ++ "..."
       lf <- readFile filename 
       let l = lParser (lexer lf)
       putStr ((show $ length l) ++ " entries\n")
       modifyIORef pst (\x -> x{le = l})
       return ()
\end{code}

\subsubsection{XML grammar}
\paragraph{loadGrammarXml}

Given the pointer to the monadic state pst, it reads and parses the
grammar from the macro file indicated in the monad.  The grammar is
storded as a hashing function in the monad.

\begin{code}
loadGrammarXml :: PState -> IO ()
loadGrammarXml pst = do 
  st <- readIORef pst
  let config   = pa st
      filename = grammarXmlFile config 
  putStr $ "Loading XML Grammar " ++ filename ++ "...\n"
  gf <- readFile filename
  let g = parseXmlGrammar gf 
  modifyIORef pst (\x -> x{tags = g})
  return ()
\end{code}

\subsection{Target semantics}

loadTargetSem: Given 
   - a pointer pst to the general state st, 
it access the parameters and the name of the file for the target
semantics from params.  parses the file and assigns the target
semantics to the ts field of st 

\begin{code}
loadTargetSem :: PState -> IO ()
loadTargetSem pst = do
       st <- readIORef pst
       let filename = tsFile (pa st)
       putStr $ "Loading Target Semantics " ++ filename ++ "...\n"
       tstr <- readFile filename
       loadTargetSemStr pst tstr
\end{code}

loadTargetSemStr: Given a string with some semantics, it parses
the string and assigns the assigns the target semantics to the ts field
of st 

\begin{code}
loadTargetSemStr :: PState -> String -> IO ()
loadTargetSemStr pst str = 
    do putStr "Parsing Target Semantics...\n"
       let sem = (tsParser (lexer str))
       case sem of 
         Ok s       -> modifyIORef pst (\x -> x{ts = flattenTargetSem s})
         Failed s   -> fail s
\end{code}

\paragraph{flattenTargetSem} takes a recursively embedded target
semantics like \verb$love(me wear(you sw))$ \verb$sweater(sw)$
and converts it into a flat semantics with handles like
\verb$love(h1 me h1.3)$ $wear(h1.3 you)$ $sweater(h2 sw)$

\begin{code}
flattenTargetSem :: [Tree String] -> Sem
flattenTargetSem trees = sortSem results
  where results  = concat $ snd $ unzip results'
        results' = map fn $ zip [1..] trees 
        fn (g,k) = flattenTargetSem' [g] k
\end{code}

\paragraph{flattenTargetSem'} We walk the semantic tree, returning an index and
a list of predicates representing the flat semantics for the entire semantic
tree.  Normally, only the second result is interesting for you; the first
result is used for recursion, the idea being that a node's parameters is the
list of indices returned by its children.  If a child does not have any
children of its own, then its index is its string value (like \verb$john$); if
it does have children, then we return a handle for it (like \verb$h1.3.4$)

\begin{code}
flattenTargetSem' :: [Int] -> Tree String -> (String, Sem)
flattenTargetSem' _  (Node predicate []) = (predicate, []) 

flattenTargetSem' gorn (Node predicate kids) =
  let smooshGorn = 'h' : (concat $ intersperse "." $ map show gorn)
      -- recursive step
      kidGorn   = map (\x -> (x:gorn)) [1..] 
      next      = zip kidGorn kids
      nextRes   = map (\ (g,k) -> flattenTargetSem' g k) next
      (kidIndexes, kidSem) = unzip nextRes
      -- create the predicate
      result     = (smooshGorn, predicate, kidIndexes)
  in (smooshGorn, result:(concat kidSem))
\end{code}

% --------------------------------------------------------------------
\section{Returning results}
% --------------------------------------------------------------------

We provide a data structure to be used by verboseGeni for returning the results
(grDerived) along with the intermediary steps and some useful statistics.  

\begin{code}
data GeniResults = GR {
  -- candidate selection
  grCand     :: [TagElem],
  -- modification of candidate selection
  -- grAuts     :: [(String,PolAut,PolAut)],
  grFinalAut :: PolAut,
  -- paths through the automaton, if any
  grCombos   :: [[TagElem]],
  -- optimisations and extra polarities
  grOptStr   :: (String,String),
  -- some numbers (in string form)
  grStats    :: Gstats,
  grAutPaths :: String,
  grTimeStr  :: String,
  -- the final results
  grDerived  :: [TagElem]
} 
\end{code}

We provide a default means of displaying the results

\begin{code}
instance Show GeniResults where
  show gres = 
    let gstats = grStats gres 
        gopts  = grOptStr gres
    in    "Optimisations: " ++ fst gopts ++ snd gopts ++ "\n"
       ++ "\nAutomaton paths explored: " ++ (grAutPaths gres)
       ++ "\nTotal agenda size: " ++ (show $ geniter gstats) 
       ++ "\nTotal chart size:  " ++ (show $ szchart gstats) 
       ++ "\nComparisons made:  " ++ (show $ numcompar gstats)
       ++ "\nGeneration time:  " ++ (grTimeStr gres)
       ++ "\n\nRealisations:\n" ++ (showRealisations $ grDerived gres)
\end{code}

\paragraph{showOptResults} displays a list of performance results in a
single table.  The intention is for each item in the list to be the 
result of a different optimisation on the same grammar/semantics

\begin{code}
showOptResults :: [GeniResults] -> String
showOptResults grs = 
  let headOpt = "         optimisations"
      headNumRes = "rslts"
      headAgenda = "agnd sz"
      headChart  = "chrt sz"
      headComparisons = "compared"
      headTime = "time ms"
      header   = [ headOpt, headNumRes, headAgenda, headChart, headComparisons, headTime ] 
      showIt l = concat $ intersperse " | " $ l
      showLine = concat $ intersperse "-+-" $ map linestr header
      resStr r = [ pad (fst  $ grOptStr r) headOpt,
                   pad (show $ length $ grDerived r) headNumRes,
                   pad (show $ geniter s) headAgenda,
                   pad (show $ szchart s) headChart,
                   pad (show $ numcompar s) headComparisons, 
                   pad (grTimeStr r) headTime ]
                 where s = grStats r
      -- a list of "-" with the same length as l 
      linestr str2 = map (const '-') str2
      -- pad str to be as long as str2
      pad str str2 = if (diff > 0) then padding ++ str else str
                     where padding = map (const ' ') [1..diff]
                           diff = (length str2) - (length str)   
      --
      headerStr = showIt header ++ "\n" ++ showLine ++ "\n" 
      bodyStr   = concat $ intersperse "\n" $ map (showIt.resStr) grs
  in headerStr ++ bodyStr
\end{code}

\paragraph{showRealisations} shows the sentences produced by the generator in a
relatively compact form 

\begin{code}
showRealisations :: [TagElem] -> String
showRealisations derived =
  let sentences        = map showLeaves derived
      sentencesGrouped = map (\s -> head s ++ count s) g
                         where g = group $ sort sentences 
      count s = if (length s > 1) 
                then " (" ++ (show $ length s) ++ " instances)"
                else ""
  in if (null derived)
     then "(No results found)"
     else concat $ intersperse "\n" $ sentencesGrouped
\end{code}


