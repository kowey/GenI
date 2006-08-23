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
module NLP.GenI.Geni (ProgState(..), ProgStateRef, emptyProgState,
             showRealisations, groupAndCount,
             initGeni, runGeni, Selector,
             loadGrammar, loadLexicon, 
             loadTestSuite, loadTargetSemStr,
             combine,

             -- used by auxiliary tools only
             chooseLexCand,
             )
where
\end{code}

\ignore{
\begin{code}
import Control.Monad.Error
import Control.Monad (unless)

import Data.IORef (IORef, readIORef, modifyIORef)
import Data.List (intersperse, sort, nub, partition)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe)

import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.Parsec 
-- import System.Process 

import Statistics (Statistics)

import NLP.GenI.General(filterTree, groupAndCount, multiGroupByFM,
    geniBug,
    repNodeByNode,
    wordsBy,
    fst3,
    ePutStr, ePutStrLn, eFlush,
    )

import NLP.GenI.Btypes
  (Macros, MTtree, ILexEntry, Lexicon,
   Replacable(..),
   Sem, SemInput, sortSem, subsumeSem, params,
   GeniVal, fromGVar,
   GNode(ganchor, gnname, gup, gdown), Flist,
   isemantics, ifamname, iword, iparams, iequations,
   iinterface, ifilters,
   isempols,
   toKeys,
   showLexeme, showPairs,
   pidname, pfamily, pinterface, ptype, psemantics, ptrace,
   setLexeme, tree, unifyFeat,
   alphaConvert,
   )

import NLP.GenI.Tags (Tags, TagElem, emptyTE,
             idname, ttreename,
             ttype, tsemantics, ttree, tsempols,
             tinterface, ttrace,
             setTidnums) 

import NLP.GenI.Configuration
  ( Params, getFlagP, hasFlagP, hasOpt, Optimisation(NoConstraints)
  , MacrosFlg(..), LexiconFlg(..), TestSuiteFlg(..), TestCaseFlg(..),
  , MorphInfoFlg(..), MorphCmdFlg(..),
  , IgnoreSemanticsFlg(..), ServerModeFlg(..), VerboseModeFlg(..),
  , grammarType,
  , GrammarType(..) )

import qualified NLP.GenI.Builder as B

import NLP.GenI.GeniParsers (geniMacros, geniTagElems,
                    geniLexicon, geniTestSuite,
                    geniTestSuiteString, geniSemanticInput,
                    geniMorphInfo)
import NLP.GenI.Morphology
-- import CkyBuilder 
-- import SimpleBuilder (simpleBuilder)
\end{code}
}

\begin{code}
myEMPTY :: String
myEMPTY = "MYEMPTY" 
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

\begin{code}
data ProgState = ST{pa     :: Params,
                    --
                    gr       :: Macros,
                    le       :: Lexicon,
                    morphinf :: MorphFn,
                    ts       :: SemInput, 
                    -- names of test case to run
                    tcase    :: String, 
                    -- name, original string (for gui), sem
                    tsuite   :: [(String,String,SemInput)]
               }

type ProgStateRef = IORef ProgState

-- | The program state when you start GenI for the very first time
emptyProgState :: Params -> ProgState
emptyProgState args =
 ST { pa = args
    , gr = []
    , le = Map.empty
    , morphinf = const Nothing
    , ts = ([],[],[])
    , tcase = []
    , tsuite = [] }
\end{code}

% --------------------------------------------------------------------
\section{Interface}
\subsection{Loading and parsing}
% --------------------------------------------------------------------

\fnlabel{loadGrammar} Given the pointer to the monadic state pstRef it reads
and parses everything it needs to get going, that is, the macros file, 
lexicon file and test suite.

\begin{code}
loadGrammar :: ProgStateRef -> IO() 
loadGrammar pstRef =
  do pst <- readIORef pstRef
     --
     let config   = pa pst
         isMissing f = not $ hasFlagP f config
     -- grammar type
         isNotPreanchored = grammarType config /= PreAnchored
         isNotPrecompiled = grammarType config /= PreCompiled
         isNotServer = isMissing ServerModeFlg
     -- display 
     let errormsg =
           concat $ intersperse ", " [ msg | (con, msg) <- errorlst, con ]
         errorlst =
              [ (isNotPrecompiled && isMissing MacrosFlg,
                "a tree file")
              , (isNotPreanchored && isMissing LexiconFlg,
                "a lexicon file")
              , (isNotServer && isMissing TestSuiteFlg,
                "a test suite") ]
     unless (null errormsg) $ fail ("Please specify: " ++ errormsg)
     -- we only have to read in grammars from the simple format
     case grammarType config of 
        PreAnchored -> return ()
        PreCompiled -> return ()
        _        -> loadGeniMacros pstRef config
     -- we don't have to read in the lexicon if it's already pre-anchored
     when isNotPreanchored $ loadLexicon pstRef config
     -- in any case, we have to...
     loadMorphInfo pstRef config 
     when isNotServer $ loadTestSuite pstRef
\end{code}

\fnlabel{loadLexicon} Given the pointer to the monadic state pstRef and
the parameters from a grammar index file parameters; it reads and parses
the lexicon file.   

\begin{code}
loadLexicon :: ProgStateRef -> Params -> IO ()
loadLexicon pstRef config =
 case getFlagP LexiconFlg config of
 Nothing    -> fail "Please specify a lexicon!"
 Just lfile ->
    do let verbose = hasFlagP VerboseModeFlg config
       when verbose $ do
         ePutStr $ "Loading Lexicon " ++ lfile ++ "..."
         eFlush
       pst <- readIORef pstRef
       let getSem l  = if hasFlagP IgnoreSemanticsFlg (pa pst) then [] else isemantics l
           sorter l  = l { isemantics = (sortSem . getSem) l }
           cleanup   = (mapBySemKeys isemantics) . (map sorter)
           --
       prelex <- parseFromFile geniLexicon lfile
       let theLex = case prelex of
                      Left err -> error (show err)
                      Right x  -> cleanup x
       --
       when verbose $ ePutStr ((show $ length $ Map.keys theLex) ++ " entries\n")
       -- combine the two lexicons
       modifyIORef pstRef (\x -> x{le = theLex})

       return ()
\end{code}

\fnlabel{loadGeniMacros} Given the pointer to the monadic state pstRef and
the parameters from a grammar index file parameters; it reads and parses
macros file.  The macros are stored as a hashing function in the monad.

\begin{code}
loadGeniMacros :: ProgStateRef -> Params -> IO ()
loadGeniMacros pstRef config = 
 case getFlagP MacrosFlg config of
 Nothing    -> fail "Please specify a trees file!"
 Just mfile ->
  do let verbose = hasFlagP VerboseModeFlg config
     when verbose $ do
       ePutStr $ "Loading Macros " ++ mfile ++ "..."
       eFlush
     parsed <- parseFromFile geniMacros mfile
     case parsed of 
       Left  err -> fail (show err)
       Right g   -> do
          when verbose $ ePutStr $ show (length g) ++ " trees\n"
          modifyIORef pstRef (\x -> x{gr = g})
\end{code}

\fnlabel{loadMorphInfo} Given the pointer to the monadic state pstRef and
the parameters from a grammar index file parameters; it reads and parses
the morphological information file, if available.  The results are stored
as a lookup function in the monad.

\begin{code}
loadMorphInfo :: ProgStateRef -> Params -> IO ()
loadMorphInfo pstRef config = 
 case getFlagP MorphInfoFlg config of
 Nothing       -> return ()
 Just filename -> do
        ePutStr $ "Loading Morphological Info " ++ filename ++ "..."
        eFlush
        parsed <- parseFromFile geniMorphInfo filename
        let g = case parsed of 
                  Left err -> fail (show err)
                  Right  x -> x
            sizeg  = length g
        ePutStr $ show sizeg ++ " entries\n" 
        modifyIORef pstRef (\x -> x{morphinf = readMorph g})
\end{code}

\subsubsection{Target semantics}

\fnlabel{loadTestSuite} 
given a pointer pstRef to the general state st, it access the parameters and the
name of the file for the target semantics from params.  It parses the file as a
test suite, and assigns it to the tsuite field of st.

\begin{code}
loadTestSuite :: ProgStateRef -> IO ()
loadTestSuite pstRef = do
  pst <- readIORef pstRef
  case getFlagP TestSuiteFlg (pa pst) of
   Nothing -> fail "Please specify a test suite!"
   Just filename -> do
     let config = pa pst
         verbose = hasFlagP VerboseModeFlg config
     unless (hasFlagP IgnoreSemanticsFlg config) $
      do when verbose $ do
           ePutStr $ "Loading Test Suite " ++ filename ++ "...\n"
           eFlush
         -- ugh: I'm using CPS here, when what I really want is
         -- some kind of Error-based monad transformer
         parseAnd geniTestSuite $ \sem ->
           parseAnd geniTestSuiteString $ \mStrs ->
             modifyIORef pstRef $ updateTsuite config sem mStrs
         return ()
     where -- helper functions for test suite stuff
      parseAnd p cont =
        do mr <- parseFromFile p filename
           case mr of
             Left  e -> fail (show e)
             Right r -> cont r
      updateTsuite config s s2 x =
        x { tsuite = zipWith cleanup s s2
          , tcase  = fromMaybe "" $ getFlagP TestCaseFlg config}
      cleanup (i, (sm, sr, lc), _) str =
        let newsmsr = (sortSem sm, sort sr, lc)
        in  (i, str, newsmsr)
\end{code}

\fnlabel{loadTargetSemStr} Given a string with some semantics, it
parses the string and assigns the assigns the target semantics to the 
ts field of the ProgState 

\begin{code}
loadTargetSemStr :: ProgStateRef -> String -> IO ()
loadTargetSemStr pstRef str = 
    do pst <- readIORef pstRef
       if hasFlagP IgnoreSemanticsFlg (pa pst) then return () else parseSem
    where
       parseSem = do
         let sem = runParser geniSemanticInput () "" str
         case sem of
           Left  err -> fail (show err)
           Right sr  -> modifyIORef pstRef (\x -> x{ts = smooth sr})
       smooth (s,r,l) = (sortSem s, sort r, l)
\end{code}

% --------------------------------------------------------------------
\subsection{Surface realisation - entry point}
% --------------------------------------------------------------------

\fnlabel{runGeni} is what you call if the only thing you want to do is run the
surface realiser.  It assumes that you have already loaded in your grammar and
parsed your input semantics, and performs the following four steps:

\begin{enumerate}
\item It initialises the realiser (lexical selection, among other things),
      via \fnref{initGeni}
\item It runs the builder (the surface realisation engine proper)
\item It unpacks the builder results 
\item It finalises the results (morphological generation)
\end{enumerate}

It returns a list of sentences, a set of Statistics, and the generator state.
The generator state is mostly useful for debugging via the graphical interface.

\begin{code}
-- | If in doubt about the Maybe Selector argument, just pass in Nothing;
--   it is only used for instances of GenI where the grammar is compiled
--   directly into GenI.
type Selector = ProgState -> IO ([TagElem],[ILexEntry])
runGeni :: ProgStateRef -> B.Builder st it Params -> IO ([String], Statistics, st)
runGeni pstRef builder =
  do let run    = B.run builder
         unpack = B.unpack builder
     -- step 1
     initStuff <- initGeni pstRef
     --
     pst <- readIORef pstRef
     let config  = pa pst
         -- step 2 
         (finalSt, stats) = run initStuff config
         -- step 3
         uninflected = unpack finalSt
     -- step 4
     sentences <- finaliseResults pstRef uninflected 
     return (sentences, stats, finalSt)
\end{code}

% --------------------------------------------------------------------
\subsection{Surface realisation - sub steps}
% --------------------------------------------------------------------

Below are the initial and final steps of \fnreflite{runGeni}.  These functions
are seperated out so that they may be individually called from the graphical
debugger.  The middle steps (running and unpacking the builder) depend on your
builder implementation.

\fnlabel{initGeni} performs lexical selection and strips the input semantics of
any morpohological literals

\begin{code}
initGeni :: ProgStateRef -> IO (B.Input)
initGeni pstRef =
 do -- disable constraints if the NoConstraintsFlg anti-optimisation is active
    modifyIORef pstRef
      (\p -> if hasOpt NoConstraints (pa p)
             then p { ts = (fst3 (ts p),[],[]) }
             else p)
    -- lexical selection
    pstLex <- readIORef pstRef
    (cand, lexonly) <- runLexSelection pstLex
    -- strip morphological predicates
    let (tsem,tres,lc) = ts pstLex
        tsem2 = stripMorphSem (morphinf pstLex) tsem
            --
    let initStuff = B.Input 
          { B.inSemInput = (tsem2, tres, lc)
          , B.inLex   = lexonly 
          , B.inCands = map (\c -> (c,-1)) cand
          }
    return initStuff 
\end{code}

\fnlabel{finaliseResults} for the moment consists only of running the
morphological generator, but there could conceivably be more involved.

\begin{code}
finaliseResults :: ProgStateRef -> [B.UninflectedSentence] -> IO [String]
finaliseResults = runMorph 
\end{code}

% --------------------------------------------------------------------
\subsection{Displaying results}
% --------------------------------------------------------------------

\paragraph{showRealisations} shows the sentences produced by the
generator in a relatively compact form 

\begin{code}
showRealisations :: [String] -> String
showRealisations sentences =
  let sentencesGrouped = map (\ (s,c) -> s ++ countStr c) g
                         where g = groupAndCount sentences 
      countStr c = if c > 1 then " (" ++ show c ++ " instances)"
                            else ""
  in if null sentences
     then "(none)"
     else concat $ intersperse "\n" $ sentencesGrouped
\end{code}

%\paragraph{describeOpts} concisely describes the optimisations used
%by GenI in a short string.  Each optimisation is described as a one
%to three letter code:
%
%\begin{code}
%describeOpts :: Params -> String
%describeOpts config =
%  let polkey = "pol"
%      polplus  = ""
%       ++ "A" -- always detects polarities
%       ++ (if polsig  config then "S" else "")
%       ++ (if chartsharing config then "C" else "")
%      --
%      adjkey = "adj"
%      adjplus  = ""   
%       ++ (if semfiltered config then "S" else "")
%       ++ "O" -- always uses ordered adjunction
%       ++ (if footconstr  config then "F" else "")
%      --
%      optPol = if polarised config then polkey ++ polStuff else ""
%        where polStuff = if null polplus then "" else ":" ++ polplus
%      optAdj = if null adjplus then "" else " " ++ adjkey ++ ":" ++ adjplus
%      optAll = optPol ++ optAdj
% in if null optAll then "none" else optAll
%\end{code}

% --------------------------------------------------------------------
\section{Lexical selection}
\label{sec:candidate_selection} \label{sec:lexical_selecetion} \label{par:lexSelection}
% --------------------------------------------------------------------

\paragraph{runLexSelection} \label{fn:runLexSelection} determines which
candidates trees which will be used to generate the current target semantics.  
In addition to the anchored candidate trees, we also return the lexical items 
themselves.  This list of lexical items is useful for debugging a grammar; 
it lets us know if GenI managed to lexically select something, but did not 
succeed in anchoring it.

\begin{code}
runLexSelection :: ProgState -> IO ([TagElem], [ILexEntry])
runLexSelection pst =
 do -- select lexical items first 
    let (tsem,_,litConstrs) = ts pst
        lexicon  = le pst
        lexCand   = chooseLexCand lexicon tsem
    -- then anchor these lexical items to trees
    let combineWithGr l =
         do let (errs, res) = combineList (gr pst) l
                isEnrichErr (EnrichError _ _ _) = True
                isEnrichErr _ = False
                (enrichEs, otherEs) = partition isEnrichErr errs
            unless (null enrichEs) $ do
                let errLocs = map eeLocation enrichEs
                    hasMatch (a,_) = any (== parsePathEq a) errLocs
                ePutStrLn $ "Warning: enrichment failures\t"
                            ++ (show $ length enrichEs) ++ " members of " ++ (ifamname l)
                            ++ " vs.\t" ++ (showLexeme.iword $ l)
                            ++ " [" ++ (showPairs $ filter hasMatch $ iequations l) ++ "] "
            mapM (ePutStrLn.show) otherEs

            -- FIXMENOW when (not.null $ errs) $ ePutStrLn (unlines errs)
            return res
    cand <- case (grammarType $ pa pst) of  
              PreAnchored  -> readPreAnchored pst
              _            -> concat `liftM` mapM combineWithGr lexCand
    -- attach any morphological information to the candidates
    let morphfn  = morphinf pst
        cand2    = attachMorph morphfn tsem cand 
    -- filter out candidates which have a semantics that does not
    -- subsume the input semantics, or which do not fulfill the
    -- trace constraints
    -- this is in case the grammar introduces literals into the
    -- semantics that weren't associated with the lexical entry
    let subsetSem t = all (`elem` tsem) $ tsemantics t
        matchesLc t = all (`elem` myTrace) constrs
          where constrs = concat [ cs | (l,cs) <- litConstrs, l `elem` mySem ]
                mySem   = tsemantics t
                myTrace = ttrace t
        cand3 = filter matchesLc $ filter subsetSem cand2
    -- FIXME: should we tell the user that we are doing this?
    -- assign ids to each candidate
    let cand4 = setTidnums cand3
    return (cand4, lexCand)
\end{code}

\paragraph{chooseLexCand} selects and returns the set of entries from
the lexicon whose semantics subsumes the input semantics. 

\begin{code}
chooseLexCand :: Lexicon -> Sem -> [ILexEntry]
chooseLexCand slex tsem = 
  let -- the initial "MYEMPTY" takes care of items with empty semantics
      keys = myEMPTY:(toKeys tsem)   
      -- we choose candidates that match keys
      lookuplex t = Map.findWithDefault [] t slex
      cand  = concatMap lookuplex keys
      -- and refine the selection... 
      cand2 = chooseCandI tsem cand
      -- treat synonyms as a single lexical entry
      cand3 = mergeSynonyms cand2
  in cand3
\end{code}

With a helper function, we refine the candidate selection by
instatiating the semantics, at the same time filtering those which
do not stay within the target semantics, and finally eliminating 
the duplicates.

\begin{code}
chooseCandI :: Sem -> [ILexEntry] -> [ILexEntry]
chooseCandI tsem cand =
  let replaceLex i (sem,sub) = 
        (replace sub i) { isemantics = sem }
      --
      helper :: ILexEntry -> [ILexEntry]
      helper l = if null sem then [l]
                 else map (replaceLex l) psubsem
        where psubsem = subsumeSem tsem sem
              sem = isemantics l
      --
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

\fnlabel{mergeSynonyms} is a factorisation technique that uses
atomic disjunction to merge all synonyms into a single lexical
entry.  Two lexical entries are considered synonyms if their
semantics match and they point to the same tree families.

\begin{code}
mergeSynonyms :: [ILexEntry] -> [ILexEntry]
mergeSynonyms lexEntry =
  let mergeFn l1 l2 = l1 { iword = (iword l1) ++ (iword l2) }
      keyFn l = (ifamname l, isemantics l)   
      synMap = foldr helper Map.empty lexEntry
        where helper x acc = Map.insertWith mergeFn (keyFn x) x acc 
  in Map.elems synMap
\end{code}

% --------------------------------------------------------------------
\subsection{Basic anchoring}
\label{sec:combine_macros}
% --------------------------------------------------------------------

This section of the code helps you to combined a selected lexical item with
a macro or a list of macros.  This is a process that can go fail for any
number of reasons, so we try to record the possible failures for book-keeping.

\begin{code}
data LexCombineError =
        BoringError String
      | EnrichError { eeMacro    :: MTtree
                    , eeLexEntry :: ILexEntry
                    , eeLocation :: PathEqLhs }
     | OtherError MTtree ILexEntry String

instance Error LexCombineError where
  noMsg    = strMsg "error combining items"
  strMsg s = BoringError s

instance Show LexCombineError where
 show (BoringError s)    = "Warning: " ++ s
 show (OtherError t l s) =
   "Warning: " ++ s ++ " on " ++ (pidname t) ++ "-" ++ (pfamily t) ++ " (" ++ (showLexeme $ iword l) ++ ")"
 show (EnrichError t l _)  = show (OtherError t l "enrichment error")
\end{code}

\fnlabel{combine}: Given 
\begin{itemize}
\item the Macros 
\item a list of ILexEntry (read from the Lexicon.in file) 
\end{itemize}

It creates the Tags repository combining lexical entries and
un-anchored trees from the grammar. It also unifies the parameters
used to specialize un-anchored trees and propagates additional features
given in the ILexEntry. 

We start by collecting all the features and parameters we want to combine.

\begin{code}
combine :: Macros -> Lexicon -> Tags
combine gram lexicon =
  let helper li = mapEither (combineOne li) macs
       where tn   = ifamname li
             macs = [ t | t <- gram, pfamily t == tn ]
  in Map.map (\e -> concatMap helper e) lexicon 

mapEither :: (a -> Either l r) -> [a] -> [r]
mapEither fn = mapMaybe (\x -> either (const Nothing) Just $ fn x)
\end{code}

\paragraph{combineList} takes a lexical item; it looks up the tree
families for that item, and anchors the item to the trees.  A simple
list of trees is returned.

\begin{code}
combineList :: Macros -> ILexEntry
            -> ([LexCombineError],[TagElem]) -- ^ any warnings, plus the results
combineList gram lexitem =
  case [ t | t <- gram, pfamily t == tn ] of
       []   -> ([BoringError $ "Family " ++ tn ++ " not found in Macros"],[])
       macs -> unzipEither $ map (combineOne lexitem) macs
  where tn = ifamname lexitem

unzipEither :: (Error e, Show b) => [Either e b] -> ([e], [b])
unzipEither es = helper ([],[]) es where
 helper accs [] = accs
 helper (eAcc, rAcc) (Left e : next)  = helper (e:eAcc,rAcc) next
 helper (eAcc, rAcc) (Right r : next) = helper (eAcc,r:rAcc) next
\end{code}

\paragraph{combineOne} \label{fn:combineOne} combines a single tree with its
lexical item to form a bonafide TagElem.  This process can fail, however,
because of filtering or enrichement

\begin{code}
combineOne :: ILexEntry -> MTtree -> Either LexCombineError TagElem
combineOne lexRaw eRaw = -- Maybe monad
 -- trace ("\n" ++ (show wt)) $
 do let l1 = alphaConvert "-l" lexRaw
        e1 = alphaConvert "-t" eRaw
    (l,e) <- unifyParamsWithWarning (l1,e1)
             >>= unifyInterfaceUsing iinterface
             >>= unifyInterfaceUsing ifilters -- filtering
             >>= enrichWithWarning -- enrichment
    return $ emptyTE
              { idname = (head $ iword l) ++ "_"
                         ++ pfamily e ++ showid (pidname e)
              , ttreename = pfamily e
              , ttype = ptype e
              , ttree = setLexeme (iword l) (tree e)
              , tsemantics  =
                 sortSem $ case psemantics e of
                           Nothing -> isemantics l
                           Just s  -> s
              , tsempols    = isempols l
              , tinterface  = pinterface e
              , ttrace      = ptrace e
              }
 where
  showid i = if null i then "" else ("-" ++ i)
  --
  unifyParamsWithWarning (l,t) =
   -- trace ("unify params " ++ wt) $
   let lp = iparams l
       tp = map fromGVar $ params t
       psubst = zip tp lp
   in if (length lp) /= (length tp)
      then Left $ OtherError t l $ "Parameter length mismatch"
      else Right $ (replace psubst l, replace psubst t)
  --
  unifyInterfaceUsing ifn (l,e) =
    -- trace ("unify interface" ++ wt) $
    case unifyFeat (ifn l) (pinterface e) of
    Nothing             -> Left $ OtherError e l $ "Interface unification error"
    Just (int2, fsubst) -> Right $ (replace fsubst l, e2)
                           where e2 = (replace fsubst e) { pinterface = int2 }
  --
  enrichWithWarning (l,e) =
    -- trace ("enrich" ++ wt) $
    do e2 <- enrich l e
       return (l,e2)
\end{code}

\subsubsection{CGM Enrichement}

Enrichment is a concept introduced by the common grammar manifesto
\cite{kow05CGM}, the idea being that during lexical selection, you sometimes
want to add feature structures to specific nodes in a tree.

The conventions taken by GenI for path equations are:

\begin{tabular}{|l|p{8cm}|}
\hline
\verb!interface.foo=bar! &
\fs{foo=bar} is unified into the interface (not the tree) \\
\hline
\verb!anchor.bot.foo=bar! &
\fs{foo=bar} is unified into the bottom feature of the node
which is marked anchor.  \\
\hline
\verb!toto.top.foo=bar! &
\fs{foo=bar} is unified into the top feature of node named toto \\
\hline
\verb!toto.bot.foo=bar! &
\fs{foo=bar} is unified into the bot feature of node named toto \\
\hline
\verb!anchor.foo=bar! &
same as \verb!anchor.bot.foo=bar!  \\
\hline
\verb!anc.whatever...! &
same as \verb!anchor.whatever...!  \\
\hline
\verb!top.foo=bar! &
same as \verb!anchor.top.foo=bar!  \\
\hline
\verb!bot.foo=bar! &
same as \verb!anchor.bot.foo=bar!  \\
\hline
\verb!foo=bar! &
same as \verb!anchor.bot.foo=bar!  \\
\hline
\verb!toto.foo=bar! &
same as \verb!toto.top.foo=bar! (creates a warning) \\
\hline
\end{tabular}

\begin{code}
-- | (node, top, att) (node is Nothing if anchor)
type PathEqLhs  = (String, Bool, String)

enrich :: ILexEntry -> MTtree -> Either LexCombineError MTtree
enrich l t = -- using the Maybe monad
 do -- separate into interface/anchor/named
    let name = fst3.fst
        nameIs n x = name x == n
        --
        parsed1 = map (\ (a,v) -> (parsePathEq a, v)) (iequations l)
        (intE, parsed2)  = partition (nameIs "interface") parsed1
    -- enrich the interface and everything else
    t2 <- foldM enrichInterface t intE
    -- enrich everything else
    foldM (enrichBy l) t2 parsed2
 where
  toAvPair ((_,_,a),v) = (a,v)
  enrichInterface tx en =
    do (i2, isubs) <- unifyFeat [toAvPair en] (pinterface tx)
         `catchError` (\_ -> throwError $ ifaceEnrichErr en)
       return $ (replace isubs tx) { pinterface = i2 }
  ifaceEnrichErr (loc,_) = EnrichError
    { eeMacro    = t
    , eeLexEntry = l
    , eeLocation = loc }

enrichBy :: ILexEntry -- ^ lexeme (for debugging info)
         -> MTtree
         -> (PathEqLhs, GeniVal) -- ^ enrichment eq
         -> Either LexCombineError MTtree
enrichBy lexEntry t (eqLhs, eqVal) =
 -- trace ("enrichBy " ++ (show mname)) $
 case filterTree match (tree t) of
 [a] -> do let tfeat = (if eqTop then gup else gdown) a
           (newfeat, sub) <- unifyFeat [(eqAtt,eqVal)] tfeat
                              `catchError` (\_ -> throwError enrichErr)
           let newnode = if eqTop then a {gup   = newfeat}
                                  else a {gdown = newfeat}
           return $ fixNode newnode $ replace sub t
 []  -> unsafePerformIO $ do
          ePutStrLn $ "Warning: " ++ eqName ++ " not found in tree " ++ (pidname t) ++ " (skipped)"
          return $ Right t -- to be robust, we accept if the node isn't there
 _   -> geniBug ("Tree with multiple matches in enrichBy. " ++
                 "\nTree: " ++ pidname t ++ "\nFamily: " ++ pfamily t ++
                 "\nMatching on: " ++ eqName)
 where
   (eqName, eqTop, eqAtt) = eqLhs
   fixNode n mt = mt { tree = repNodeByNode match n (tree mt) }
   match = case eqName of
           "anchor" -> ganchor
           n -> \g -> gnname g == n
   enrichErr = EnrichError { eeMacro    = t
                           , eeLexEntry = lexEntry
                           , eeLocation = eqLhs }

-- | Parse a path equation using the GenI conventions
parsePathEq :: String -> PathEqLhs
parsePathEq e =
 case wordsBy '.' e of
 (n:"top":r) -> (n, True, rejoin r)
 (n:"bot":r) -> (n, False, rejoin r)
 ("top":r) -> ("anchor", True, rejoin r)
 ("bot":r) -> ("anchor", False, rejoin r)
 ("anc":r) -> parsePathEq $ rejoin $ "anchor":r
 ("anchor":r)    -> ("anchor", False, rejoin r)
 ("interface":r) -> ("interface", False, rejoin r)
 (n:r) -> unsafePerformIO $ do
           ePutStrLn $ "Warning: Interpreting path equation " ++ e ++
                       " as applying to top of " ++ n ++ "."
           return (n, True, rejoin r)
 _ -> unsafePerformIO $ do
        ePutStrLn $ "Warning: could not interpret path equation " ++ e
        return ("", True, e) -- unknown
 where
  rejoin = concat . (intersperse ".")
\end{code}

% --------------------------------------------------------------------
\subsection{Pre-anchoring}
\label{sec:pre-anchor}
% --------------------------------------------------------------------

For debugging purposes, it is often useful to perform lexical selection and
surface realisation separately.  Pre-anchored mode allows the user to just
pass the lexical selection in as a file of anchored trees associated with a
semantics.

\begin{code}
readPreAnchored :: ProgState -> IO [TagElem]
readPreAnchored pst =
 case getFlagP MacrosFlg (pa pst) of
 Nothing   -> fail "No macros file specified (preanchored mode)"
 Just file ->
  do parsed <- parseFromFile geniTagElems file
     case parsed of
       Left err -> fail (show err)
       Right c  -> return c
\end{code}

% --------------------------------------------------------------------
\section{Morphology} 
% --------------------------------------------------------------------

\paragraph{runMorph} inflects a list of sentences if a morphlogical generator
has been specified.  If not, it returns the sentences as lemmas.

\begin{code}
runMorph :: ProgStateRef -> [[(String,Flist)]] -> IO [String]
runMorph pstRef sentences = 
  do pst <- readIORef pstRef
     case getFlagP MorphCmdFlg (pa pst) of
       Nothing  -> return (map sansMorph sentences)
       Just cmd -> inflectSentences cmd sentences
\end{code}


