\chapter{Geni}

Geni is the interface between the front and backends of the generator. The GUI
and the console interface both talk to this module, and in turn, this module
talks to the input file parsers and the surface realisation engine.  This
module also does lexical selection and anchoring because these processes might
involve some messy IO performance tricks.

\begin{code}
module Geni (State(..), PState, GeniResults(..), 
             showRealisations, showOptResults,
             initGeni, customGeni, runGeni, 
             loadGrammar, 
             loadTargetSem, loadTargetSemStr,
             -- for debugging only
             combine, chooseCand)
where
\end{code}

\ignore{
\begin{code}
import Data.List (intersect, intersperse, sort, nub, group)
import Data.Tree
import IOExts(IORef, readIORef, newIORef, modifyIORef)

import System (ExitCode(ExitSuccess), 
               exitWith, getArgs)
import System.IO(hFlush, stdout)

import FiniteMap
import Monad (when)
import CPUTime (getCPUTime)

import Btypes (Macros, MTtree, ILexEntry, Lexicon, Sem, Flist, Subst,
               GNode, GType(Subs), 
               isemantics, itreename, iword, iparams, ipfeat,
               gnname, gtype, gaconstr, gup, gdown, toKeys,
               sortSem, subsumeSem, params, 
               substSem, substFlist', substFlist,
               pidname, pfeat, ptype, 
               ptpolarities, 
               setLexeme, tree,
               groupByFM, multiGroupByFM)

import Tags (Tags, TagElem, emptyTE, TagSite, 
             idname, tidnum,
             derivation, ttype, tsemantics, ttree, 
             tpolarities, 
             substnodes, adjnodes, findInTags, 
             appendToVars, substTagElem)

import Configuration(Params, defaultParams, getConf, treatArgs,
                     grammarFile, tsFile, 
                     GramParams, parseGramIndex,
                     macrosFile, lexiconFile, semlexFile, grammarType,
                     GrammarType(TAGML), 
                     polarised, polsig, chartsharing, 
                     semfiltered, orderedadj, extrapol)

import Mstate (Gstats, numcompar, szchart, geniter, initGstats,
               addGstats, initMState, runState, genstats,
               generate)

import Polarity
import Treeprint (showLeaves)
--import Predictors (PredictorMap, mapByPredictors, 
--                   fillPredictors, optimisePredictors)

import Lex2 (lexer)
import Mparser (mParser)
import Lparser (lParser)
import Tsparser (targetSemParser, E(..))
import GrammarXml (parseXmlGrammar, parseXmlLexicon)
\end{code}
}

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
                gr      :: Macros,
                le      :: Lexicon,
                ts      :: Sem
               }

type PState = IORef State
\end{code}

% --------------------------------------------------------------------
\section{Entry point}
% --------------------------------------------------------------------

This module mainly exports two monadic functions: an initialisation step
and a generation step.

\subsection{Initialisation step}

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
                       gr = emptyFM,
                       le = emptyFM,
                       ts = []}
    return pst 
\end{code}

\subsection{Generation step}

In the generation step, we first perform lexical selection, set up any
optimisations on the lexical choices, and then perform generation.

\paragraph{customGeni} lets you specify what function you want to use for
generation: this is useful because it lets you pass in a debugger
instead of the vanilla generator.  To run the vanilla generator, 
call this function with runGeni as the runFn argument.

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
  let -- polarity optimisation (if enabled)
      isPol        = polarised config 
      (candLite, lookupCand) = reduceTags (polsig config) cand
      (_,finalaut) = makePolAut candLite tsem extraPol
      pathsLite    = walkAutomaton finalaut 
      paths        = map (concatMap lookupCand) pathsLite 
      combosPol    = if isPol then paths else [cand]
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

% --------------------------------------------------------------------
\section{Lexical selection}
\label{sec:candidate_selection}
\label{sec:lexical_selecetion}
\label{par:lexSelection}.
% --------------------------------------------------------------------

\paragraph{runLexSelection} determines which candidates trees which
will be used to generate the current target semantics.  

Note: we assign a tree id to each selected tree, and we append some
unique suffix (coincidentally the tree id) to each variable in each
selected tree. This is to avoid nasty collisions during unification as
mentioned in section \ref{sec:fs_unification}).

\begin{code}
runLexSelection :: IORef State -> IO [TagElem] 
runLexSelection pst = do
    mst <- readIORef pst
    let tsem     = ts mst
        lexicon  = le mst
        -- select lexical items first 
        lexCand   = chooseLexCand lexicon tsem
        -- then anchor these lexical items to trees
        combiner = combineList (gr mst) 
        cand     = concatMap combiner lexCand
        -- assure unique variable names
        setnum c i = appendToVars (mksuf i) (c { tidnum = i })
        mksuf i = "-" ++ (show i)
    return $ zipWith setnum cand [1..]
\end{code}


% --------------------------------------------------------------------
\subsection{Combine}
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
combine g lexicon =
  let helper li = map (combineOne li) macs 
                  where tn   = itreename li
                        macs = lookupWithDefaultFM g [] tn
  in mapFM (\_ e -> concatMap helper e) lexicon 
\end{code}

\paragraph{combineList} takes a lexical item; it looks up the tree
families for that item, and anchors the item to the trees.  A simple
list of trees is returned.

\begin{code}
combineList :: Macros -> ILexEntry -> [TagElem]
combineList g lexitem = 
  let tn = itreename lexitem
      macs = case (lookupFM g tn) of
                Just tt -> tt
                Nothing -> error ("Family " ++ tn ++ " not found in Macros")
  in map (combineOne lexitem) macs
\end{code}

\paragraph{combineOne} combines a single tree with its lexical item to
form a bonafide TagElem

\begin{code}
combineOne :: ILexEntry -> MTtree -> TagElem
combineOne lexitem e = 
   let wt = "(Word: "++ (iword lexitem) ++
            ", Family:" ++ (itreename lexitem) ++ ")\n"
       -- lexitem stuff
       sem  = isemantics lexitem
       p    = iparams lexitem
       pf   = ipfeat lexitem
       fpf  = map fst pf
       -- tree stuff
       tp   = params e
       tpf  = pfeat e
       ftpf = map fst tpf
       -- unify the Features and Parameters.
       paramsUnified = replacePar (zip tp p) (Btypes.tree e)
       (unified,snodes,anodes) = replaceFeat pf paramsUnified 
       -- the final result
       sol = emptyTE {
                idname = (iword lexitem) ++ "-" ++ (pidname e),
                derivation = (0,[]),
                ttype = ptype e,
                ttree = setLexeme (iword lexitem) unified,
                substnodes = snodes,
                adjnodes   = anodes,
                tsemantics = sem,
                tpolarities = ptpolarities e
                -- tpredictors = combinePredictors e lexitem
               }        
    in -- error checking
       if ((length p) /= (length tp))  -- if the parameters are of different length
       then error ("Wrong number of parameters. " ++ wt)
       else -- if the features specified in ILexEntry are not a subset
            -- of the ones specified in the grammar.
            if (intersect fpf ftpf /= fpf) 
            then error ("Feature atributes don't match. " ++ wt)
            else sol
\end{code}

% --------------------------------------------------------------------
\subsubsection{Replace feat}
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
\subsubsection{Instatiation of arguments}
% --------------------------------------------------------------------

\paragraph{replacePar}: Given 
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
\subsection{The selection process}
% --------------------------------------------------------------------

\paragraph{chooseCand} It access the Grammar for the candidate tags and
loads them in the Agenda.  It checks for semantics which adds more than
the required input, discarding those tags.  It also instantiates the
semantics of candidates to match the target semantics. 

See page \pageref{fn:subsumeSem} for the definition of semantic subsumption.

\begin{code}
chooseCand :: Tags -> Sem -> [TagElem]
chooseCand t tsem =
  let -- we also retrieve empty-semantic elements
      keys = "MYEMPTY":(toKeys tsem)   
      -- we choose candidates that match keys
      cand = concatMap (findInTags t) keys
      -- and then refine the selection...
  in chooseCandI tsemantics substTagElem tsem cand
\end{code}

With a helper function, we refine the candidate selection by
instatiating the semantics, at the same time filtering those which
do not stay within the target semantics, and finally eliminating 
the duplicates.

\begin{code}
chooseCandI :: (Eq a) => (a->Sem) -> (a->Subst->a) 
                         -> Sem -> [a] -> [a]
chooseCandI semfn substfn tsem cand =
  let psubst te = if (null sem) then [[]] else subsumeSem tsem sem
                  where sem = semfn te
      helper te = map (substfn te) (psubst te)
  in nub $ concatMap helper cand
\end{code}

% --------------------------------------------------------------------
\subsubsection{Lexicon-only selection}
% --------------------------------------------------------------------

An alternative to candidate selection (of trees) is to do lexical
selection (just the lexical items), and combine these lexically 
selected items with the grammar.

\begin{code}
chooseLexCand :: Lexicon -> Sem -> [ILexEntry]
chooseLexCand slex tsem = 
  let substLex i sub = i { isemantics = substSem (isemantics i) sub
                         , ipfeat     = substFlist (ipfeat i)   sub  
                         , iparams    = substPar  (iparams i)   sub
                         }
      substPar par sub = map (\p -> foldl sfn p sub) par
                         where sfn z (x,y) = if (z == x) then y else z
      -- the initial "MYEMPTY" takes care of items with empty semantics
      keys = "MYEMPTY":(toKeys tsem)   
      -- we choose candidates that match keys
      lookuplex t = lookupWithDefaultFM slex [] t
      cand    = concatMap lookuplex keys
      -- and refine the selection... 
  in chooseCandI isemantics substLex tsem cand
\end{code}

\paragraph{mapBySemKeys} organises items by their semantic key.  A
semantic key is a semantic literal boiled down to predicate plus arity
(see section \ref{btypes_semantics}).  Given \texttt{xs} a list of items
and \texttt{fn} a function which retrieves the item's semantics, we
return a FiniteMap from semantic key to a list of items with that key.
An item may have multiple keys.

This is used to organise the lexicon by its semantics.

\begin{code}
mapBySemKeys :: (a -> Sem) -> [a] -> FiniteMap String [a]
mapBySemKeys semfn xs = 
  let gfn t = if (null s) then ["MYEMPTY"] else toKeys s 
              where s = semfn t
  in multiGroupByFM gfn xs
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

There are two types of GenI grammars.  Hand-written grammars have a
simple human-friendly format.  Automatically generated grammars are
built from a meta-grammar compiler and have an XML syntax.  The grammar
index file will indicate what kind of grammar we have.

Note: the semantic lexicon still uses the GeniHand format

\paragraph{loadGrammar} Given the pointer to the monadic state pst it
reads and parses the grammar file index; and from this information,
it reads the rest of the grammar (macros, lexicon, etc).  The Macros
and the Lexicon 

\begin{code}
loadGrammar :: PState -> IO() 
loadGrammar pst =
  do st <- readIORef pst
     --
     let config   = pa st
         filename = grammarFile config
     -- 
     putStr $ "Loading index file " ++ filename ++ "..."
     hFlush stdout
     gf <- readFile filename
     putStrLn $ "done"
     --
     let gparams = parseGramIndex filename gf
     loadLexicon pst gparams
     loadMacros  pst gparams
\end{code}

\paragraph{loadLexicon} Given the pointer to the monadic state pst and
the parameters from a grammar index file parameters; it reads and parses
the lexicon file and the semantic lexicon.   These are then stored in
the mondad.

\begin{code}
loadLexicon :: PState -> GramParams -> IO ()
loadLexicon pst config = do 
       let lfilename = lexiconFile config
           sfilename = semlexFile config
           isTAGML   = (grammarType config == TAGML)
 
       putStr $ "Loading Semantic Lexicon " ++ sfilename ++ "..."
       hFlush stdout
       sf <- readFile sfilename
       let semmapper = mapBySemKeys isemantics
           semlex    = (semmapper . lParser . lexer) sf
       putStr ((show $ length $ keysFM semlex) ++ " entries\n")

       putStr $ "Loading Lexicon " 
              ++ (if isTAGML then "XML " else "")
              ++ lfilename ++ "..."
       hFlush stdout
       lf <- readFile lfilename 
       let lex' = if isTAGML
                  then parseXmlLexicon lf
                  else (lParser . lexer) lf
           lex  = groupByFM iword lex'
       putStr ((show $ length $ keysFM lex) ++ " entries\n")
       modifyIORef pst (\x -> x{le = combineLexicon semlex lex})
       return ()
\end{code}

\paragraph{combineLexicon} merges the lemma lexicon and the semantic
lexicon into a single lexicon.  The idea is that the semantic lexicon
and the lemma lexicon both use the ILexEntry data type, but they contain
different information: the semantic lexicon has the semantics and the
parameters, whereas the lemma lexicon has everything else.  Each entry
in the semantic lexicon has a semantics and a lemma.  We look the lemma
up in the (surprise!) lemma lexicon, and copy the semantic information
into each instance.

\begin{code}
combineLexicon :: Lexicon -> Lexicon -> Lexicon
combineLexicon sl ll = 
  let merge si li = li { isemantics = (isemantics si)
                       , iparams   = (iparams si) }
      helper si = map (merge si) lemmas
                  where lemmas  = lookupWithDefaultFM ll [] (iword si)
  in mapFM (\_ e -> concatMap helper e) sl 
\end{code}

\paragraph{loadMacros} Given the pointer to the monadic state pst and
the parameters from a grammar index file parameters; it reads and parses
macros file.  The macros are storded as a hashing function in the monad.

\begin{code}
loadMacros :: PState -> GramParams -> IO ()
loadMacros pst config = 
  do let filename = macrosFile config
         isTAGML  = (grammarType config == TAGML)
     --
     putStr $ "Loading Macros " 
              ++ (if isTAGML then "XML " else "") 
              ++ filename ++ "..."
     hFlush stdout
     gf <- readFile filename
     let (g, u) = if isTAGML  
                  then (parseXmlGrammar gf, [])
                  else mParser (lexer gf)
         sizeg  = sum (map length $ eltsFM g)
         errmsg  = "Some trees in grammar declared neither initial nor auxiliar.\n  Aborting.\n"
     if null u -- if no errors
        then do putStr $ show sizeg ++ " trees in " 
                putStr $ (show $ sizeFM g) ++ " families\n"
                modifyIORef pst (\x -> x{gr = g})
                return ()
        else error (errmsg ++ (show u))
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
       putStr $ "Loading Target Semantics " ++ filename ++ "..."
       tstr <- readFile filename
       loadTargetSemStr pst tstr
       putStr "done\n"
\end{code}

loadTargetSemStr: Given a string with some semantics, it parses
the string and assigns the assigns the target semantics to the ts field
of st 

\begin{code}
loadTargetSemStr :: PState -> String -> IO ()
loadTargetSemStr pst str = 
    do putStr "Parsing Target Semantics..."
       let sem = (targetSemParser . lexer) str
       case sem of 
         Ok s       -> modifyIORef pst (\x -> x{ts = flattenTargetSem s})
         Failed s   -> fail s
       putStr "done\n"
\end{code}

\paragraph{flattenTargetSem} takes a recursively embedded target
semantics like \verb$love(me wear(you sw))$ \verb$sweater(sw)$
and converts it into a flat semantics with handles like
\verb$love(h1 me h1.3)$ \verb$wear(h1.3 you)$ $sweater(h2 sw)$

\begin{code}
flattenTargetSem :: [Tree (String, String)] -> Sem
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
flattenTargetSem' :: [Int] -> Tree (String,String) -> (String, Sem)
flattenTargetSem' _  (Node (_,pred) []) = (pred, []) 

flattenTargetSem' gorn (Node (hand,pred) kids) =
  let smooshGorn = "gh" ++ (concat $ intersperse "." $ map show gorn)
      -- recursive step
      kidGorn   = map (\x -> (x:gorn)) [1..] 
      next      = zip kidGorn kids
      nextRes   = map (\ (g,k) -> flattenTargetSem' g k) next
      (kidIndexes, kidSem) = unzip nextRes
      -- create the predicate
      handle    = if null hand then smooshGorn else hand
      result    = (handle, pred, kidIndexes)
  in (smooshGorn, result:(concat kidSem))
\end{code}

% --------------------------------------------------------------------
\section{Generation step} 
% --------------------------------------------------------------------

Actually running the generator...  Note: the only reason this is monadic
is to be compatible with the debugger GUI.  There could be some code
simplifications in order.

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

\subsection{Returning results}

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
       ++ "\nGeneration time:  " ++ (grTimeStr gres) ++ " ms"
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


