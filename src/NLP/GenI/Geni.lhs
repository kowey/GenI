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
             initGeni, runGeni, runGeniWithSelector, getTraces, GeniResult, Selector,
             loadEverything, loadLexicon, loadGeniMacros,
             loadTestSuite, loadTargetSemStr,
             loadRanking, readRanking,
             combine,

             -- used by auxiliary tools only
             chooseLexCand,
             )
where
\end{code}

\ignore{
\begin{code}
import Control.Arrow (first,(&&&))
import Control.Monad.Error
import Control.Monad (unless)

import Data.Binary (Binary, decodeFile)
import Data.Function ( on )
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.List
import Data.List.Split ( wordsBy )
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe, isJust)
import Data.Tree (Tree(Node))
import Data.Typeable (Typeable)

import qualified System.IO.UTF8 as UTF8

import System.IO.Unsafe (unsafePerformIO)
import Text.JSON
-- import System.Process 


import NLP.GenI.General(filterTree, repAllNode,
    groupAndCount, multiGroupByFM,
    geniBug,
    repNodeByNode,
    fst3,
    ePutStr, ePutStrLn, eFlush,
    )

import NLP.GenI.Btypes
  (Macros, MTtree, ILexEntry, Lexicon,
   replace, replaceList,
   Sem, SemInput, TestCase(..), sortSem, subsumeSem, params,
   GeniVal(GConst), fromGVar, AvPair(..),
   GNode(ganchor, gnname, gup, gdown, gaconstr, gtype, gorigin), Flist,
   GType(Subs, Other),
   isemantics, ifamname, iword, iparams, iequations,
   iinterface, ifilters,
   isempols,
   toKeys,
   showLexeme, showSem,
   pidname, pfamily, pinterface, ptype, psemantics, ptrace,
   setAnchor, setLexeme, tree, unifyFeat,
   alphaConvert,
   )
import NLP.GenI.BtypesBinary ()

import NLP.GenI.Tags (Tags, TagElem, emptyTE,
             idname, ttreename,
             ttype, tsemantics, ttree, tsempols,
             tinterface, ttrace,
             setTidnums) 

import NLP.GenI.Configuration
  ( Params, getFlagP, hasFlagP, hasOpt, Optimisation(NoConstraints)
  , MacrosFlg(..), LexiconFlg(..), TestSuiteFlg(..), TestCaseFlg(..)
  , MorphInfoFlg(..), MorphCmdFlg(..)
  , RankingConstraintsFlg(..)
  , PartialFlg(..)
  , FromStdinFlg(..), VerboseModeFlg(..)
  , NoLoadTestSuiteFlg(..)
  , TracesFlg(..)
  , grammarType
  , GrammarType(..) )

import qualified NLP.GenI.Builder as B

import NLP.GenI.GeniParsers (geniMacros, geniTagElems,
                    geniLexicon, geniTestSuite,
                    geniTestSuiteString, geniSemanticInput,
                    geniMorphInfo,
                    parseFromFile, runParser, Parser,
                    )
import NLP.GenI.Morphology
import NLP.GenI.OptimalityTheory
import NLP.GenI.Statistics (Statistics)

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

\begin{code}
data ProgState = ST{ -- | the current configuration being processed
                    pa     :: Params,
                    --
                    gr       :: Macros,
                    le       :: Lexicon,
                    morphinf :: MorphFn,
                    ts       :: SemInput, 
                    -- | names of test case to run
                    tcase    :: String, 
                    -- | name, original string (for gui), sem
                    tsuite   :: [TestCase],
                    -- | OT constraints (optional)
                    ranking  :: OtRanking,
                    -- | simplified traces (optional)
                    traces   :: [String],
                    -- | any warnings accumulated during realisation
                    --   (most recent first)
                    warnings :: [String]
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
    , tsuite = []
    , traces = []
    , ranking = []
    , warnings = []
    }

-- | Log another warning in our internal program state
addWarning :: ProgStateRef -> String -> IO ()
addWarning pstRef s = modifyIORef pstRef $ \p -> p { warnings = s : warnings p }
\end{code}

% --------------------------------------------------------------------
\section{Interface}
\subsection{Loading and parsing}
% --------------------------------------------------------------------

We have one master function that loads all the files GenI is expected to
use.  This just calls the sub-loaders below, some of which are exported
for use by the graphical interface.  The master function also makes sure
to complain intelligently if some of the required files are missing.

\begin{code}
loadEverything :: ProgStateRef -> IO() 
loadEverything pstRef =
  do pst <- readIORef pstRef
     --
     let config   = pa pst
         isMissing f = not $ hasFlagP f config
     -- grammar type
         isNotPreanchored = grammarType config /= PreAnchored
         isNotPrecompiled = grammarType config /= PreCompiled
         useTestSuite =  isMissing FromStdinFlg
                      && isMissing NoLoadTestSuiteFlg
     -- display 
     let errormsg =
           concat $ intersperse ", " [ msg | (con, msg) <- errorlst, con ]
         errorlst =
              [ (isNotPrecompiled && isMissing MacrosFlg,
                "a tree file")
              , (isNotPreanchored && isMissing LexiconFlg,
                "a lexicon file")
              , (useTestSuite && isMissing TestSuiteFlg,
                "a test suite") ]
     unless (null errormsg) $ fail ("Please specify: " ++ errormsg)
     -- we only have to read in grammars from the simple format
     case grammarType config of 
        PreAnchored -> return ()
        PreCompiled -> return ()
        _        -> loadGeniMacros pstRef
     -- we don't have to read in the lexicon if it's already pre-anchored
     when isNotPreanchored $ loadLexicon pstRef
     -- in any case, we have to...
     loadMorphInfo pstRef
     when useTestSuite $ loadTestSuite pstRef >> return ()
     -- the trace filter file
     loadTraces pstRef
     -- OT ranking
     loadRanking pstRef
\end{code}

The file loading functions all work the same way: we load the file,
and try to parse it.  If this doesn't work, we just fail in IO, and
GenI dies.  If we succeed, we update the program state passed in as
an IORef.

\begin{code}
loadLexicon :: ProgStateRef -> IO ()
loadLexicon pstRef =
    do let getSem l  = isemantics l
           sorter l  = l { isemantics = (sortSem . getSem) l }
           cleanup   = mapBySemKeys isemantics . map sorter
       xs <- loadThingOrDie LexiconFlg "lexicon" pstRef
         (parseFromFileOrFail geniLexicon)
       modifyIORef pstRef (\p -> p { le = cleanup xs })

-- | The macros are stored as a hashing function in the monad.
loadGeniMacros :: ProgStateRef -> IO ()
loadGeniMacros pstRef =
  do xs <- loadThingOrDie MacrosFlg "trees" pstRef parser
     modifyIORef pstRef (\p -> p { gr = xs })
  where parser = parseFromFileMaybeBinary geniMacros

-- | The results are stored as a lookup function in the monad.
loadMorphInfo :: ProgStateRef -> IO ()
loadMorphInfo pstRef =
 do xs <- loadThingOrIgnore MorphInfoFlg "morphological info" pstRef parser
    modifyIORef pstRef (\p -> p { morphinf = readMorph xs } )
 where parser = parseFromFileOrFail geniMorphInfo

loadTraces :: ProgStateRef -> IO ()
loadTraces pstRef =
 do xs <- loadThingOrIgnore TracesFlg "traces" pstRef
             (\f -> lines `fmap` readFile f)
    modifyIORef pstRef (\p -> p {traces = xs})

loadRanking :: ProgStateRef -> IO ()
loadRanking pstRef =
 do config <- pa `fmap` readIORef pstRef
    let verbose = hasFlagP VerboseModeFlg config
    case getFlagP RankingConstraintsFlg config of
      Nothing -> return ()
      Just f  -> do r <- readRanking verbose f
                    modifyIORef pstRef (\p -> p { ranking = r })

readRanking :: Bool -- ^ verbose
            -> FilePath -> IO OtRanking
readRanking verbose f =
 do when verbose $ do
       ePutStr $ unwords [ "Loading OT constraints", f ++ "... " ]
       eFlush
    mr <- (resultToEither . decode) `fmap` UTF8.readFile f -- utf-8?
    when verbose $ ePutStr "done"
    either fail return mr
\end{code}

\subsubsection{Target semantics}

Reading in the target semantics (or test suite) is a little more
complicated.  It follows the same general schema as above, except
that we parse the file twice: once for our internal representation,
and once to get a string representation of each test case.  The
string representation is for the graphical interface; it avoids us
figuring out how to pretty-print things because we can assume the
user will format it the way s/he wants.

\begin{code}
-- | Stores the results in the tcase and tsuite fields
loadTestSuite :: ProgStateRef -> IO [TestCase]
loadTestSuite pstRef = do
  config <- pa `fmap` readIORef pstRef
  let parser f = do
         sem   <- parseFromFileOrFail geniTestSuite f
         mStrs <- parseFromFileOrFail geniTestSuiteString f
         return $ zip sem mStrs
      updater s x =
        x { tsuite = s
          , tcase  = fromMaybe "" $ getFlagP TestCaseFlg config}
      cleanup (tc,str) =
        tc { tcSem = (sortSem sm, sort sr, lc)
           , tcSemString = str }
        where (sm, sr, lc) = tcSem tc
  xs <- map cleanup `fmap` loadThingOrDie TestSuiteFlg "test suite" pstRef parser
  modifyIORef pstRef (updater xs)
  return xs
\end{code}

Sometimes, the target semantics does not come from a file, but from
the graphical interface, so we also provide the ability to parse an
arbitrary string as the semantics.

\begin{code}
-- | Updates program state the same way as 'loadTestSuite'
loadTargetSemStr :: ProgStateRef -> String -> IO ()
loadTargetSemStr pstRef str = 
    do parseSem
    where
       parseSem = do
         let sem = runParser geniSemanticInput () "" str
         case sem of
           Left  err -> fail (show err)
           Right sr  -> modifyIORef pstRef (\x -> x{ts = smooth sr})
       smooth (s,r,l) = (sortSem s, sort r, l)
\end{code}

\subsubsection{Helpers for loading files}

\begin{code}
loadThingOrIgnore, loadThingOrDie :: forall f a . (Eq f, Show f, Typeable f)
           => (FilePath -> f) -- ^ flag
           -> String
           -> ProgStateRef
           -> (FilePath -> IO [a])
           -> IO [a]

-- | Load the file if the relevant option is set, otherwise ignore
loadThingOrIgnore flag description pstRef parser =
 do config <- pa `fmap` readIORef pstRef
    case getFlagP flag config of
      Nothing -> return []
      Just f  -> loadThing f description pstRef parser

-- | Load the file if the relevant option is set, otherwise complain and die
loadThingOrDie flag description pstRef parser =
 do config <- pa `fmap` readIORef pstRef
    case getFlagP flag config of
      Nothing -> fail $ "Please specify a " ++ description ++ "!"
      Just f  -> loadThing f description pstRef parser

loadThing :: FilePath             -- ^ file to load
          -> String               -- ^ description
          -> ProgStateRef
          -> (FilePath -> IO [a]) -- ^ parsing cmd
          -> IO [a]
loadThing filename description pstRef parser =
 do config <- pa `fmap` readIORef pstRef
    let verbose = hasFlagP VerboseModeFlg config
    when verbose $ do
       ePutStr $ unwords [ "Loading",  description, filename ++ "... " ]
       eFlush
    theTs <- parser filename
    when verbose $ ePutStr $ (show $ length theTs) ++ " entries\n"
    return theTs

parseFromFileOrFail :: Parser a -> FilePath -> IO a
parseFromFileOrFail p f = parseFromFile p f >>= either (fail.show) (return)

parseFromFileMaybeBinary :: Binary a
                         => Parser a
                         -> FilePath
                         -> IO a
parseFromFileMaybeBinary p f =
 if (".genib" `isSuffixOf` f)
    then decodeFile f
    else parseFromFileOrFail p f
\end{code}

% --------------------------------------------------------------------
\subsection{Surface realisation - entry point}
% --------------------------------------------------------------------

This is your basic entry point.  You call this if the only thing you want to do
is run the surface realiser.

\begin{enumerate}
\item It initialises the realiser (lexical selection, among other things),
      via \fnref{initGeni}
\item It runs the builder (the surface realisation engine proper)
\item It unpacks the builder results 
\item It finalises the results (morphological generation)
\end{enumerate}

\begin{code}
type GeniResult = (String, B.Derivation)

-- | Returns a list of sentences, a set of Statistics, and the generator state.
--   The generator state is mostly useful for debugging via the graphical interface.
--   Note that we assumes that you have already loaded in your grammar and
--   parsed your input semantics.
runGeni :: ProgStateRef -> B.Builder st it Params -> IO ([GeniResult], Statistics, st)
runGeni pstRef builder = runGeniWithSelector pstRef defaultSelector builder

runGeniWithSelector :: ProgStateRef -> Selector -> B.Builder st it Params -> IO ([GeniResult], Statistics, st)
runGeniWithSelector pstRef  selector builder =
  do let run    = B.run builder
         unpack = B.unpack builder
         getPartial = B.partial builder
     -- step 1
     initStuff <- initGeniWithSelector pstRef selector
     --
     pst <- readIORef pstRef
     let config  = pa pst
         -- step 2 
         (finalSt, stats) = run initStuff config
         -- step 3
         uninflected = unpack finalSt
         partial = getPartial finalSt
     -- step 4
     sentences <- if null uninflected && hasFlagP PartialFlg config
                     then map (first star) `fmap` finaliseResults pstRef partial
                     else finaliseResults pstRef uninflected
     return (sentences, stats, finalSt)
 where star :: String -> String
       star s = '*' : s
\end{code}

% --------------------------------------------------------------------
\subsection{Surface realisation - sub steps}
% --------------------------------------------------------------------

Below are the initial and final steps of \fnreflite{runGeni}.  These functions
are seperated out so that they may be individually called from the graphical
debugger.  The middle steps (running and unpacking the builder) depend on your
builder implementation.

\begin{code}
-- | 'initGeni' performs lexical selection and strips the input semantics of
--   any morpohological literals
initGeni :: ProgStateRef -> IO (B.Input)
initGeni pstRef = initGeniWithSelector pstRef defaultSelector

initGeniWithSelector :: ProgStateRef -> Selector -> IO (B.Input)
initGeniWithSelector pstRef lexSelector =
 do -- disable constraints if the NoConstraintsFlg anti-optimisation is active
    modifyIORef pstRef
      (\p -> if hasOpt NoConstraints (pa p)
             then p { ts = (fst3 (ts p),[],[]) }
             else p)
    -- lexical selection
    (cand, lexonly) <- lexSelector pstRef
    pst <- readIORef pstRef
    -- strip morphological predicates
    let (tsem,tres,lc) = ts pst
        tsem2 = stripMorphSem (morphinf pst) tsem
            --
    let initStuff = B.Input 
          { B.inSemInput = (tsem2, tres, lc)
          , B.inLex   = lexonly 
          , B.inCands = map (\c -> (c,-1)) cand
          }
    return initStuff 
\end{code}

\begin{code}
-- | 'finaliseResults' for the moment consists only of running the
--   morphological generator, but there could conceivably be more involved.
finaliseResults :: ProgStateRef -> [B.Output] -> IO [GeniResult]
finaliseResults pstRef os =
 do mss <- runMorph pstRef ss
    return . concat $ zipWith merge mss ds
 where
    (ss,ds) = unzip os
    merge ms d = map (\m -> (m,d)) ms
\end{code}

% --------------------------------------------------------------------
\subsection{Displaying results}
% --------------------------------------------------------------------

\begin{code}
-- | Show the sentences produced by the generator, in a relatively compact form
showRealisations :: [String] -> String
showRealisations sentences =
  let sentencesGrouped = map (\ (s,c) -> s ++ countStr c) g
                         where g = groupAndCount sentences 
      countStr c = if c > 1 then " (" ++ show c ++ " instances)"
                            else ""
  in if null sentences
     then "(none)"
     else unlines sentencesGrouped
\end{code}

\begin{code}
-- | 'getTraces' is most likely useful for grammars produced by a
--   metagrammar system.  Given a tree name, we retrieve the ``trace''
--   information from the grammar for all trees that have this name.  We
--   assume the tree name was constructed by GenI; see the source code for
--   details.
getTraces :: ProgState -> String -> [String]
getTraces pst tname =
  filt $ concat [ ptrace t | t <- gr pst, pidname t == readPidname tname ]
  where
   filt = case traces pst of
          []    -> id
          theTs -> filter (`elem` theTs)

-- | We assume the name was constructed by 'combineName'
readPidname :: String -> String
readPidname n =
  case wordsBy (== ':') n of
  (_:_:p:_) -> p
  _         -> geniBug "readPidname or combineName are broken"
\end{code}

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
runLexSelection :: ProgStateRef -> IO ([TagElem], [ILexEntry])
runLexSelection pstRef =
 do pst <- readIORef pstRef
    -- select lexical items first
    let (tsem,_,litConstrs) = ts pst
        lexicon  = le pst
        lexCand   = chooseLexCand lexicon tsem
        config   = pa pst
        verbose  = hasFlagP VerboseModeFlg config
    -- then anchor these lexical items to trees
    let grammar = gr pst
        combineWithGr l =
         do let (lexCombineErrors, res) = combineList grammar l
                familyMembers = [ p | p <- grammar, pfamily p == ifamname l ]
            mapM_ (addWarning pstRef . showErr) $ compressLexCombineErrors
                                                $ lexCombineErrors
            -- snippets of error message
            let lexeme = showLexeme.iword $ l
                _outOfFamily n = show n ++ "/" ++ (show $ length familyMembers)
                                 ++ " instances of " ++ lexeme ++ ":" ++ ifamname l
            -- print out missing coanchors list
            case concatMap (missingCoanchors l) familyMembers of
              [] -> return ()
              cs -> mapM_ showWarning . group . sort $ cs
                    where showWarning [] = geniBug "silly error in Geni.runLexSelection"
                          showWarning xs@(x0:_) = addWarning pstRef $ "Missing co-anchor '" ++ x0 ++ "'" ++ " in " ++ _outOfFamily (length xs) ++ "."
            -- print out enrichment errors
{-
            unless (null enrichEs) $ do
                let numDiscards = length enrichEs
                    badEnrichments = [ av | av <- iequations l, hasMatch av ]
                    hasMatch (a,_) = any (== parsePathEq a) errLocs
                    errLocs = map eeLocation enrichEs
                ePutStrLn $      "Warning: Discarded "
                            ++ _outOfFamily numDiscards
                            ++ "\n         due to enrichment failure with "
                            ++ "[" ++ showPairs badEnrichments ++ "]."
            mapM (ePutStrLn.show) otherEs
-}

            -- FIXMENOW when (not.null $ errs) $ ePutStrLn (unlines errs)
            return res
    cand <- case grammarType config of
              PreAnchored  -> readPreAnchored pst
              _            -> concat `liftM` mapM combineWithGr lexCand
    -- attach any morphological information to the candidates
    let considerMorph = attachMorph (morphinf pst) tsem
    -- filter out candidates which do not fulfill the trace constraints
    let matchesLc t = all (`elem` myTrace) constrs
          where constrs = concat [ cs | (l,cs) <- litConstrs, l `elem` mySem ]
                mySem   = tsemantics t
                myTrace = ttrace t
        considerLc = filter matchesLc
    -- filter out candidates whose semantics has bonus stuff which does
    -- not occur in the input semantics
    let considerCoherency = filter (all (`elem` tsem) . tsemantics)
        considerHasSem    = filter (not . null . tsemantics)
    --
    let candFinal = setTidnums . considerCoherency . considerHasSem
                  . considerLc . considerMorph $ cand
        indent  x = ' ' : x
        unlinesIndentAnd :: (x -> String) -> [x] -> String
        unlinesIndentAnd f = unlines . map (indent . f)
    when verbose $
      do ePutStrLn $ "Lexical items selected:\n" ++ (unlinesIndentAnd (showLexeme.iword) lexCand)
         ePutStrLn $ "Trees anchored (family) :\n" ++ (unlinesIndentAnd idname candFinal)
    -- lexical selection failures
    let missedSem  = tsem \\ (nub $ concatMap tsemantics candFinal)
        hasTree l = isJust $ find (\t -> tsemantics t == lsem) cand
          where lsem = isemantics l
        missedLex = filter (not.hasTree) lexCand
    unless (null missedSem) $ addWarning pstRef $ "no lexical selection for " ++ showSem missedSem
    unless (null missedLex) $ forM_ missedLex $ \l -> addWarning pstRef $
        "'" ++ showLex l ++ "' was lexically selected, but not anchored to any trees"
    return (candFinal, lexCand)
 where showLex l = (showLexeme $ iword l) ++ "-" ++ (ifamname l)
       showErr (c, e) = show e ++ " (" ++ show c ++ " times)"

compressLexCombineErrors :: [LexCombineError] -> [(Int, LexCombineError)]
compressLexCombineErrors = map (length &&& head) . groupBy h
 where
  h (EnrichError m1 l1 _) (EnrichError m2 l2 _) = pfamily m1 == pfamily m2 &&
                                                  iword l1 == iword l2
  h _ _ = False

-- | Select and returns the set of entries from the lexicon whose semantics
--   subsumes the input semantics.
chooseLexCand :: Lexicon -> Sem -> [ILexEntry]
chooseLexCand slex tsem = 
  let keys = toKeys tsem
      -- we choose candidates that match keys
      lookuplex t = Map.findWithDefault [] t slex
      cand  = concatMap lookuplex $ myEMPTY : keys
      -- and refine the selection... 
      cand2 = chooseCandI tsem cand
      -- treat synonyms as a single lexical entry
      -- FIXME: disabled see mergeSynonyms for explanation
      -- cand3 = mergeSynonyms cand2
  in cand2
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

A semantic key is a semantic literal boiled down to predicate plus arity
(see section \ref{btypes_semantics}).


\begin{code}
-- | 'mapBySemKeys' @xs fn@ organises items (@xs@) by their semantic key
--   (retrieved by @fn@).  An item may have multiple keys.
---  This is used to organise the lexicon by its semantics.
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

FIXME: 2006-10-11 - note that this is no longer being used,
because it breaks the case where two lexical entries differ
only by their use of path equations.  Perhaps it's worthwhile
just to add a check that the path equations match exactly.

\begin{code}
{-
mergeSynonyms :: [ILexEntry] -> [ILexEntry]
mergeSynonyms lexEntry =
  let mergeFn l1 l2 = l1 { iword = (iword l1) ++ (iword l2) }
      keyFn l = (ifamname l, isemantics l)   
      synMap = foldr helper Map.empty lexEntry
        where helper x acc = Map.insertWith mergeFn (keyFn x) x acc 
  in Map.elems synMap
-}
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
 show (BoringError s)    = s
 show (OtherError t l s) = s ++ " on " ++ pfamily t ++ " (" ++ (showLexeme $ iword l) ++ ")"
 show (EnrichError t l _) = show (OtherError t l "enrichment error")
\end{code}

The first step in lexical selection is to collect all the features and
parameters that we want to combine.

\begin{code}
-- | 'combine' @macros lex@ creates the 'Tags' repository combining lexical
--   entries and un-anchored trees from the grammar. It also unifies the
--   parameters used to specialize un-anchored trees and propagates additional
--   features given in the 'ILexEntry'.
combine :: Macros -> Lexicon -> Tags
combine gram lexicon =
  let helper li = mapEither (combineOne li) macs
       where tn   = ifamname li
             macs = [ t | t <- gram, pfamily t == tn ]
  in Map.map (\e -> concatMap helper e) lexicon 

mapEither :: (a -> Either l r) -> [a] -> [r]
mapEither fn = mapMaybe (\x -> either (const Nothing) Just $ fn x)
\end{code}

\begin{code}
-- | Given a lexical item, looks up the tree families for that item, and
--   anchor the item to the trees.
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

\begin{code}
-- | Combine a single tree with its lexical item to form a bonafide TagElem.
--   This process can fail, however, because of filtering or enrichement
combineOne :: ILexEntry -> MTtree -> Either LexCombineError TagElem
combineOne lexRaw eRaw = -- Maybe monad
 -- trace ("\n" ++ (show wt)) $
 do let l1 = alphaConvert "-l" lexRaw
        e1 = alphaConvert "-t" eRaw
    (l,e) <- unifyParamsWithWarning (l1,e1)
             >>= unifyInterfaceUsing iinterface
             >>= unifyInterfaceUsing ifilters -- filtering
             >>= enrichWithWarning -- enrichment
    let name = concat $ intersperse ":" $ filter (not.null)
                 [ head (iword l) , pfamily e , pidname e ]
    return $ emptyTE
              { idname = name
              , ttreename = pfamily e
              , ttype = ptype e
              , ttree = setOrigin name . setLemAnchors . setAnchor (iword l) $ tree e
              , tsemantics  =
                 sortSem $ case psemantics e of
                           Nothing -> isemantics l
                           Just s  -> s
              , tsempols    = isempols l
              , tinterface  = pinterface e
              , ttrace      = ptrace e
              }
 where
  unifyParamsWithWarning (l,t) =
   -- trace ("unify params " ++ wt) $
   let lp = iparams l
       tp = map fromGVar $ params t
       psubst = zip tp lp
   in if (length lp) /= (length tp)
      then Left $ OtherError t l $ "Parameter length mismatch"
      else Right $ (replaceList psubst l, replaceList psubst t)
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

\subsubsection{Enrichment}

Enrichment is a process which adds features to either the interface, an
explicitly named node or the co-anchor of a lexically selected tree.  The
enrichement information comes from the lexicon in the form of a path equations
which specify
\begin{enumerate}
\item the location
\item top or bottom
\item the attribute
\item what value to associate with it
\end{enumerate}

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
type PathEqPair = (PathEqLhs, GeniVal)

enrich :: ILexEntry -> MTtree -> Either LexCombineError MTtree
enrich l t =
 do -- separate into interface/anchor/named
    let (intE, namedE) = lexEquations l
    -- enrich the interface and everything else
    t2 <- foldM enrichInterface t intE
    -- enrich everything else
    foldM (enrichBy l) t2 namedE
 where
  toAvPair ((_,_,a),v) = AvPair a v
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
 case seekCoanchor eqName t of
 Nothing -> return t -- to be robust, we accept if the node isn't there
 Just a  ->
        do let tfeat = (if eqTop then gup else gdown) a
           (newfeat, sub) <- unifyFeat [AvPair eqAtt eqVal] tfeat
                              `catchError` (\_ -> throwError enrichErr)
           let newnode = if eqTop then a {gup   = newfeat}
                                  else a {gdown = newfeat}
           return $ fixNode newnode $ replace sub t
 where
   (eqName, eqTop, eqAtt) = eqLhs
   fixNode n mt = mt { tree = repNodeByNode (matchNodeName eqName) n (tree mt) }
   enrichErr = EnrichError { eeMacro    = t
                           , eeLexEntry = lexEntry
                           , eeLocation = eqLhs }

pathEqName :: PathEqPair -> String
pathEqName = fst3.fst

missingCoanchors :: ILexEntry -> MTtree -> [String]
missingCoanchors lexEntry t =
  -- list monad
  do eq <- nubBy ((==) `on` pathEqName) $ snd $ lexEquations lexEntry
     let name = pathEqName eq
     case seekCoanchor name t of
       Nothing -> [name]
       Just _  -> []

-- | Split a lex entry's path equations into interface enrichement equations
--   or (co-)anchor modifiers
lexEquations :: ILexEntry -> ([PathEqPair], [PathEqPair])
lexEquations =
  partition (nameIs "interface") . map parseAv . iequations
  where
   parseAv (AvPair a v) =
    case parsePathEq a of
      Left (err,peq) -> unsafePerformIO $ do putStrLn err
                                             return (peq,v)
      Right peq -> (peq, v)
   nameIs n x = pathEqName x == n

seekCoanchor :: String -> MTtree -> Maybe GNode
seekCoanchor eqName t =
 case filterTree (matchNodeName eqName) (tree t) of
 [a] -> Just a
 []  -> Nothing
 _   -> geniBug $ "Tree with multiple matches in enrichBy. " ++
                  "\nTree: " ++ pidname t ++ "\nFamily: " ++ pfamily t ++
                  "\nMatching on: " ++ eqName

matchNodeName :: String -> GNode -> Bool
matchNodeName "anchor" = ganchor
matchNodeName n        = (== n) . gnname

-- | Parse a path equation using the GenI conventions
--   This always succeeds, but can return @Just warning@
--   if anything anomalous comes up
parsePathEq :: String -> Either (String,PathEqLhs) (PathEqLhs)
parsePathEq e =
  case wordsBy (== '.') e of
  (n:"top":r) -> Right (n, True, rejoin r)
  (n:"bot":r) -> Right (n, False, rejoin r)
  ("top":r) -> Right ("anchor", True, rejoin r)
  ("bot":r) -> Right ("anchor", False, rejoin r)
  ("anc":r) -> parsePathEq $ rejoin $ "anchor":r
  ("anchor":r)    -> Right ("anchor", False, rejoin r)
  ("interface":r) -> Right ("interface", False, rejoin r)
  (n:r) -> Left (err, (n, True, rejoin r))
           where err = "Warning: Interpreting path equation " ++ e ++
                       " as applying to top of " ++ n ++ "."
  _ -> Left (err, ("", True, e))
       where err = "Warning: could not interpret path equation " ++ e
 where
  rejoin = concat . intersperse "."
\end{code}

\subsubsection{Lemanchor mechanism}

One problem in building reversible grammars is the treatment of co-anchors.
In the French language, for example, we have some structures like
\natlang{C'est Jean qui regarde Marie}
\natlang{It is John who looks at Mary}

One might be tempted to hard code the ce (it) and the être (is) into the tree
for regarder (look at), something like \texttt{s(ce, être, n$\downarrow$, qui,
v(regarder), n$\downarrow$)}.  Indeed, this would work just fine for
generation, but not for parsing.  When you parse, you would encounter inflected
forms for these items for example \natlang{c'} for \natlang{ce} or
\natlang{sont} or \natlang{est} for \natlang{être}.  Hard-coding the \natlang{ce}
into such trees would break parsing.

To work around this, we propose a mechanism to have our co-anchors and parsing
too. Co-anchors that are susceptible to morphological variation should be
\begin{itemize}
\item marked in a substitution site (this is to keep parsers happy)
\item have a feature \texttt{bot.lemanchor:foo} where foo is the
      coanchor you want
\end{itemize}

GenI will convert these into non-substitution sites with a lexical item
leaf node.

\begin{code}
setLemAnchors :: Tree GNode -> Tree GNode
setLemAnchors t =
 repAllNode fn filt t
 where
  filt (Node a []) = gtype a == Subs && (isJust. lemAnchor) a
  filt _ = False
  fn (Node x k) = setLexeme (lemAnchorMaybeFake x) $
                    Node (x { gtype = Other, gaconstr = False }) k
  --
  lemAnchorMaybeFake :: GNode -> [String]
  lemAnchorMaybeFake n =
    case lemAnchor n of
    Nothing -> ["ERR_UNSET_LEMMANCHOR"]
    Just l  -> l
  lemAnchor :: GNode -> Maybe [String]
  lemAnchor n =
    case [ v | AvPair a v <- gdown n, a == _lemanchor ] of
    [GConst l] -> Just l
    _          -> Nothing

_lemanchor :: String
_lemanchor = "lemanchor"
\end{code}

\subsubsection{Node origins}

After lexical selection, we label each tree node with its origin, most
likely the name and id of its elementary tree.  This is useful for
building derivation trees

\begin{code}
setOrigin :: String -> Tree GNode -> Tree GNode
setOrigin t = fmap (\g -> g { gorigin = t })
\end{code}

% --------------------------------------------------------------------
\subsection{Pre-selection and pre-anchoring}
\label{sec:pre-anchor}
% --------------------------------------------------------------------

For testing purposes, we can perform lexical selection ahead of time and store
it somewhere else.

\begin{code}
-- | Only used for instances of GenI where the grammar is compiled
--   directly into GenI.
type Selector = ProgStateRef -> IO ([TagElem],[ILexEntry])

defaultSelector :: Selector
defaultSelector = runLexSelection
\end{code}

For debugging purposes, it is often useful to perform lexical selection and
surface realisation separately.  Pre-anchored mode allows the user to just
pass the lexical selection in as a file of anchored trees associated with a
semantics.

\begin{code}
readPreAnchored :: ProgState -> IO [TagElem]
readPreAnchored pst =
 case getFlagP MacrosFlg (pa pst) of
 Nothing   -> fail "No macros file specified (preanchored mode)"
 Just file -> parseFromFileOrFail geniTagElems file
\end{code}

% --------------------------------------------------------------------
\section{Morphology} 
% --------------------------------------------------------------------

\begin{code}
-- | 'runMorph' inflects a list of sentences if a morphlogical generator
-- has been specified.  If not, it returns the sentences as lemmas.
runMorph :: ProgStateRef -> [[(String,Flist)]] -> IO [[String]]
runMorph pstRef sentences = 
  do pst <- readIORef pstRef
     let sentences2 = map (map (uncurry B.LemmaPlus)) sentences
     case getFlagP MorphCmdFlg (pa pst) of
       Nothing  -> return $ map sansMorph sentences2
       Just cmd -> map snd `fmap` inflectSentencesUsingCmd cmd sentences2
\end{code}


