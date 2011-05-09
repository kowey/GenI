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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module NLP.GenI.Geni (
             -- * main interface
             ProgState(..), ProgStateRef, emptyProgState,
             initGeni,
             runGeni, runGeniWithSelector,
             GeniResult(..), ResultType(..),
             -- * helpers
             lemmaSentenceString, prettyResult,
             showRealisations, groupAndCount,
             getTraces, Selector,
             loadEverything,
             loadLexicon, Loadable(..),
             loadGeniMacros,
             loadTestSuite, loadTargetSemStr,
             loadRanking,

             -- used by auxiliary tools only
             chooseLexCand,
             )
where
\end{code}

\ignore{
\begin{code}
import Control.Applicative ((<$>),(<*>))
import Control.Monad.Error

import Data.Binary (Binary, decodeFile)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.List
import Data.List.Split ( wordsBy )
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Typeable (Typeable)

import System.CPUTime( getCPUTime )
import NLP.GenI.Statistics
import Control.Parallel.Strategies

import qualified System.IO.UTF8 as UTF8

import Text.JSON
-- import System.Process 

import NLP.GenI.General(
    groupAndCount,
    geniBug,
    fst3, snd3,
    ePutStr, ePutStrLn, eFlush,
    )

import NLP.GenI.Btypes
  (Macros, ILexEntry, Lexicon,
   SemInput, Sem, LitConstr, TestCase(..), sortSem,
   isemantics, ifamname, iword,
   showLexeme, showSem,
   pidname, pfamily, ptrace,
   )
import NLP.GenI.BtypesBinary ()

import NLP.GenI.Tags (TagElem,
             idname,
             tsemantics,
             ttrace,
             setTidnums) 

import NLP.GenI.Configuration
  ( Params, getFlagP, hasFlagP, hasOpt, Optimisation(NoConstraints)
  , MacrosFlg(..), LexiconFlg(..), TestSuiteFlg(..), TestCaseFlg(..)
  , MorphInfoFlg(..), MorphCmdFlg(..)
  , RankingConstraintsFlg(..)
  , PartialFlg(..)
  , FromStdinFlg(..), VerboseModeFlg(..)
  , NoLoadTestSuiteFlg(..)
  , RootFeatureFlg(..)
  , TracesFlg(..)
  , grammarType
  , GrammarType(..) )

import qualified NLP.GenI.Builder as B

import NLP.GenI.GeniParsers (geniMacros, geniTagElems,
                    geniLexicon, geniTestSuite,
                    geniTestSuiteString, geniSemanticInput,
                    geniMorphInfo,
                    parseFromFile, runParser, Parser,
                    ParseError,
                    )
import NLP.GenI.LexicalSelection
        ( mapBySemKeys, chooseLexCand, combineList, compressLexCombineErrors, LexCombineError
        , missingCoanchors,
        )
import NLP.GenI.Morphology
import NLP.GenI.OptimalityTheory
-- import CkyBuilder 
-- import SimpleBuilder (simpleBuilder)

-- -- DEBUG
-- import Control.Monad.Writer
-- import NLP.GenI.Lexicon
-- import NLP.GenI.LexicalSelection
-- import NLP.GenI.FeatureStructures
\end{code}
}

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
              [ (isMissing RootFeatureFlg,
                "a root feature [empty feature is fine if you are not using polarity filtering]")
              , (isNotPrecompiled && isMissing MacrosFlg,
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
        _        -> loadGeniMacros pstRef >> return ()
     -- we don't have to read in the lexicon if it's already pre-anchored
     when isNotPreanchored $ loadLexicon pstRef >> return ()
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
class Loadable x where
  lParse       :: String -> Either ParseError x
  lSet         :: x -> ProgState -> ProgState
  lSummarise   :: x -> String

-- | Note that here we assume the input consists of UTF-8 encoded file
lParseFromFile :: Loadable x => FilePath -> IO (Either ParseError x)
lParseFromFile = fmap lParse
               . UTF8.readFile

-- | Returns the input too (convenient for type checking)
lSetState :: Loadable x => ProgStateRef -> x -> IO x
lSetState pstRef x = modifyIORef pstRef (lSet x) >> return x

-- to be phased out
dieOnParseError :: Either ParseError x -> IO x
dieOnParseError (Left err) = fail (show err)
dieOnParseError (Right p)  = return p

data L a = Loadable a => L
\end{code}

\begin{code}
-- | Load something, exiting GenI if we have not been given the
--   appropriate flag
loadOrDie :: forall f a . (Eq f, Show f, Typeable f, Loadable a)
          => L a
          -> (FilePath -> f) -- ^ flag
          -> String
          -> ProgStateRef
          -> IO a
loadOrDie L flg descr pstRef =
  withFlagOrDie flg pstRef descr $ \f -> do
   v <- verbosity pstRef
   x <- withLoadStatus v f descr lParseFromFile
     >>= dieOnParseError
     >>= lSetState pstRef
   return x

-- | Load something from a string rather than a file
loadFromString :: Loadable a => ProgStateRef -> String -> IO a
loadFromString pstRef s =
  dieOnParseError (lParse s) >>= lSetState pstRef

instance Loadable Lexicon where
  lParse = fmap toLexicon . runParser geniLexicon () ""
    where
     toLexicon = mapBySemKeys isemantics . map sorter
     sorter l  = l { isemantics = (sortSem . isemantics) l }
  lSet x p = p { le = x }
  lSummarise x = show (Map.size x) ++ " lemmas"

instance Loadable Macros where
  lParse   = runParser geniMacros () ""
  lSet x p = p { gr = x }
  lSummarise x = show (length x) ++ " schemata"

loadLexicon :: ProgStateRef -> IO Lexicon
loadLexicon = loadOrDie (L :: L Lexicon) LexiconFlg "lexicon"

-- | The macros are stored as a hashing function in the monad.
loadGeniMacros :: ProgStateRef -> IO Macros
loadGeniMacros pstRef =
  withFlagOrDie MacrosFlg pstRef descr $ \f -> do
     v <- verbosity pstRef
     withLoadStatus v f descr (parseFromFileMaybeBinary lParseFromFile)
     >>= dieOnParseError
     >>= lSetState pstRef
  where
   descr = "trees"
\end{code}

\begin{code}
-- | Load something, but only if we are configured to do so
loadOptional :: forall f a . (Eq f, Show f, Typeable f, Loadable a)
             => L a
             -> (FilePath -> f) -- ^ flag
             -> String
             -> ProgStateRef
             -> IO ()
loadOptional L flg descr pstRef =
  withFlagOrIgnore flg pstRef $ \f -> do
   v <- verbosity pstRef
   x <- withLoadStatus v f descr lParseFromFile
     >>= dieOnParseError
     >>= lSetState pstRef
   let _ = x :: a
   return () -- ignore

newtype MorphFnL = MorphFnL MorphFn

instance Loadable MorphFnL where
  lParse = fmap (MorphFnL . readMorph) . runParser geniMorphInfo () ""
  lSet (MorphFnL x) p = p { morphinf = x }
  lSummarise _ = "morphinfo"

newtype TracesL = TracesL [String]

instance Loadable TracesL where
 lParse = Right . TracesL . lines
 lSet (TracesL xs) p = p { traces = xs }
 lSummarise (TracesL xs) = show (length xs) ++ " traces"

instance Loadable OtRanking where
  lParse   = resultToEither2 . decode
  lSet r p = p { ranking = r }
  lSummarise _ = "ranking"

loadMorphInfo :: ProgStateRef -> IO ()
loadMorphInfo = loadOptional (L :: L MorphFnL) MorphInfoFlg "morphological info"

loadTraces :: ProgStateRef -> IO ()
loadTraces = loadOptional (L :: L TracesL) TracesFlg "traces"

loadRanking :: ProgStateRef -> IO ()
loadRanking = loadOptional (L :: L OtRanking) RankingConstraintsFlg "OT constraints"

resultToEither2 :: Result a -> Either ParseError a
resultToEither2 r =
  case resultToEither r of
    Left e  -> runParser (fail e) () "" "" -- convoluted way to generate a Parsec error
    Right x -> Right x
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
newtype TestSuiteL = TestSuiteL [TestCase]

instance Loadable TestSuiteL where
 lParse s =
   case runParser geniTestSuite () "" s of
     Left e     -> Left e
     Right sem  -> case runParser geniTestSuiteString () "" s of
        Left e      -> Left e
        Right mStrs -> Right (TestSuiteL (zipWith cleanup sem mStrs))
   where
    cleanup tc str =
        tc { tcSem = first3 sortSem (tcSem tc)
           , tcSemString = str }
    first3 f (x, y, z) = (f x, y, z)
 --
 lSet (TestSuiteL x) p = p { tsuite = x }
 lSummarise (TestSuiteL x) = show (length x) ++ " cases"

-- | Stores the results in the tcase and tsuite fields
loadTestSuite :: ProgStateRef -> IO [TestCase]
loadTestSuite pstRef = do
  TestSuiteL xs <- loadOrDie (L :: L TestSuiteL) TestSuiteFlg "test suite" pstRef
  mtc <- (getFlagP TestCaseFlg . pa) `fmap` readIORef pstRef
  modifyIORef pstRef (\p -> p { tcase = fromMaybe "" mtc })
  return xs
\end{code}

Sometimes, the target semantics does not come from a file, but from
the graphical interface, so we also provide the ability to parse an
arbitrary string as the semantics.

\begin{code}
newtype SemL = SemL SemInput

instance Loadable SemL where
 lParse = fmap (SemL . smooth)
        . runParser geniSemanticInput () ""
   where
    smooth (s,r,l) = (sortSem s, sort r, l)
 lSet (SemL x) p = p { ts = x }
 lSummarise (SemL _) = "sem input"

loadTargetSemStr :: ProgStateRef -> String -> IO ()
loadTargetSemStr pstRef s = do
  x <- loadFromString pstRef s
  let _ = x :: SemL
  return ()
\end{code}

\subsubsection{Helpers for loading files}

\begin{code}
withFlag :: forall f a . (Eq f, Show f, Typeable f)
         => (FilePath -> f) -- ^ flag
         -> ProgStateRef
         -> IO a               -- ^ null action
         -> (FilePath -> IO a) -- ^ job
         -> IO a
withFlag flag pstRef z job =
 do config <- pa `fmap` readIORef pstRef
    case getFlagP flag config of
      Nothing -> z
      Just  x -> job x

withFlagOrIgnore :: forall f . (Eq f, Show f, Typeable f)
                 => (FilePath -> f) -- ^ flag
                 -> ProgStateRef
                 -> (FilePath -> IO ())
                 -> IO ()
withFlagOrIgnore flag pstRef = withFlag flag pstRef (return ())

withFlagOrDie :: forall f a . (Eq f, Show f, Typeable f)
              => (FilePath -> f) -- ^ flag
              -> ProgStateRef
              -> String
              -> (FilePath -> IO a)
              -> IO a
withFlagOrDie flag pstRef description = withFlag flag pstRef (fail ("Please specify a " ++ description ++ "!"))

withLoadStatus :: Loadable a
               => Bool                    -- ^ verbose
               -> FilePath             -- ^ file to load
               -> String               -- ^ description
               -> (FilePath -> IO (Either ParseError a)) -- ^ parsing cmd
               -> IO (Either ParseError a)
withLoadStatus False f _ p = p f
withLoadStatus True  f d p = do
  ePutStr $ unwords [ "Loading",  d, f ++ "... " ]
  eFlush
  mx <- p f
  ePutStrLn $ either (const "ERROR") (\x -> lSummarise x ++ " loaded") mx
  return mx

parseFromFileMaybeBinary :: Binary a
                         => (FilePath -> IO (Either ParseError a))
                         -> FilePath
                         -> IO (Either ParseError a)
parseFromFileMaybeBinary p f =
 if (".genib" `isSuffixOf` f)
    then Right `fmap` decodeFile f
    else p f
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
data GeniResult = GeniResult
 { grLemmaSentence     :: B.LemmaPlusSentence
 , grRealisations :: [String]
 , grDerivation   :: B.TagDerivation --type definition changed in Builder.hs 
 , grLexSelection :: [ GeniLexSel ]
 , grRanking      :: Int
 , grViolations   :: [ OtViolation ]
 , grResultType   :: ResultType
 , grOrigin       :: Integer -- normally a chart item id
 } deriving (Ord, Eq)

data GeniLexSel = GeniLexSel
 { nlTree  :: String
 , nlTrace :: [String]
 } deriving (Ord, Eq)

data ResultType = CompleteResult | PartialResult deriving (Ord, Eq)

-- | Returns a list of sentences, a set of Statistics, and the generator state.
--   The generator state is mostly useful for debugging via the graphical interface.
--   Note that we assumes that you have already loaded in your grammar and
--   parsed your input semantics.
runGeni :: ProgStateRef -> B.Builder st it Params -> IO ([GeniResult], Statistics, st)
runGeni pstRef builder = runGeniWithSelector pstRef defaultSelector builder

runGeniWithSelector :: ProgStateRef -> Selector -> B.Builder st it Params -> IO ([GeniResult], Statistics, st)
runGeniWithSelector pstRef  selector builder =
  do pst <- readIORef pstRef
     let config = pa pst
         run    = B.run builder
         unpack = B.unpack builder
     -- step 1: lexical selection
     initStuff <- initGeniWithSelector pstRef selector 
     start <- ( rnf initStuff ) `seq` getCPUTime  --force evaluation before measuring start time to avoid including grammar/lexicon parsing.

     -- step 2: chart generation
     let (finalSt, stats) = run initStuff config
     -- step 3: unpacking
     let uninflected = unpack finalSt
         tryPartial  = null uninflected && hasFlagP PartialFlg config
         rawResults  = if tryPartial then B.partial builder finalSt else uninflected
         resultTy    = if tryPartial then PartialResult else CompleteResult
     -- step 4: post-processing
     results <- finaliseResults pstRef resultTy rawResults
     end <-  ( rnf results ) `seq` getCPUTime --force evaluation before measuring end time to account for all the work that should be done.
     let elapsedTime = picosToMillis $! end - start
     let diff = round (elapsedTime :: Double) :: Int
     let stats2 = updateMetrics (incrIntMetric "gen_time"  (fromIntegral diff) ) stats

     return (results, stats2, finalSt)

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

-- | 'finaliseResults' does any post-processing steps that we want to integrate
--   into mainline GenI.  So far, this consists of morphological realisation and
--   OT ranking
finaliseResults :: ProgStateRef -> ResultType -> [B.Output] -> IO [GeniResult]
finaliseResults pstRef ty os =
 do pst <- readIORef pstRef
    -- morph TODO: make this a bit safer
    mss <- case getFlagP MorphCmdFlg (pa pst) of
             Nothing  -> return $ map sansMorph sentences
             Just cmd -> map snd `fmap` inflectSentencesUsingCmd cmd sentences
    -- OT ranking
    let unranked = zipWith (sansRanking pst) os mss
        rank = rankResults (getTraces pst) grDerivation (ranking pst)
    return . map addRanking . rank $ unranked
 where
  sentences = map snd3 os
  sansRanking pst (i,l,d) rs =
    GeniResult { grLemmaSentence = l
               , grRealisations = rs
               , grDerivation   = d
               , grLexSelection = map (\x -> GeniLexSel x (getTraces pst x)) (B.lexicalSelection d)
               , grRanking = -1
               , grViolations = []
               , grResultType = ty
               , grOrigin     = i
               }
  addRanking (i,res,vs) = res { grViolations = vs, grRanking = i }
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

-- | No morphology! Pretend the lemma string is a sentence
lemmaSentenceString :: GeniResult -> String
lemmaSentenceString = unwords . map lpLemma . grLemmaSentence

prettyResult :: ProgState -> GeniResult -> String
prettyResult pst nr =
  concat . intersperse "\n" . map showOne . grRealisations $ nr
 where
  showOne str = show theRanking  ++ ". " ++ str ++ "\n" ++ violations
  violations  = prettyViolations tracesFn verbose (grViolations nr)
  theRanking  = grRanking nr
  verbose  = hasFlagP VerboseModeFlg (pa pst)
  tracesFn = getTraces pst

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
    let (tsem,_,litConstrs) = ts pst
        lexicon  = le pst
        config   = pa pst
        verbose  = hasFlagP VerboseModeFlg config
        grammar = gr pst
    -- perform lexical selection
    (cand, lexCand, errs) <- case grammarType config of
                               PreAnchored -> do cs <- readPreAnchored pstRef
                                                 return (cs, [], [])
                               _           -> return $ initialLexSelection tsem lexicon grammar
    let candFinal = finaliseLexSelection (morphinf pst) tsem litConstrs cand
    -- status
    when verbose $
      do ePutStrLn $ "Lexical items selected:\n" ++ (unlinesIndentAnd (showLexeme.iword) lexCand)
         ePutStrLn $ "Trees anchored (family) :\n" ++ (unlinesIndentAnd idname candFinal)
    -- anchoring errors
    mapM_ (addWarning pstRef . showErr) $ concatMap compressLexCombineErrors errs
    -- more lexical selection errors
    forM_ lexCand $ \l ->
         do let familyMembers = [ p | p <- grammar, pfamily p == ifamname l ]
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
{-
            -- print out enrichment errors
            let isEnrichErr (EnrichError _ _ _) = True
                isEnrichErr _ = False
                (otherEs, enrichEs) = partition isEnrichErr (concat errs)
            unless (null enrichEs) $ do
                let numDiscards = length enrichEs
                    badEnrichments = [ av | av <- iequations l, hasMatch av ]
                    hasMatch (AvPair a _) = any (== (fst . runWriter $ parsePathEq a)) errLocs
                    errLocs = map eeLocation enrichEs
                ePutStrLn $      "Warning: Discarded "
                            ++ _outOfFamily numDiscards
                            ++ "\n         due to enrichment failure with "
                            ++ "[" ++ showPairs badEnrichments ++ "]."
            mapM (ePutStrLn.show) otherEs
-}
    -- lexical selection failures
    let missedSem  = tsem \\ (nub $ concatMap tsemantics candFinal)
        hasTree l = isJust $ find (\t -> tsemantics t == lsem) cand
          where lsem = isemantics l
        missedLex = filter (not.hasTree) lexCand
    unless (null missedSem) $ addWarning pstRef $ "no lexical selection for " ++ showSem missedSem
    unless (null missedLex) $ forM_ missedLex $ \l -> addWarning pstRef $
        "'" ++ showLex l ++ "' was lexically selected, but not anchored to any trees"
    return (candFinal, lexCand)
 where
   showLex l = (showLexeme $ iword l) ++ "-" ++ (ifamname l)
   showErr (c, e) = show e ++ " (" ++ show c ++ " times)"
   --
   indent  x = ' ' : x
   unlinesIndentAnd :: (x -> String) -> [x] -> String
   unlinesIndentAnd f = unlines . map (indent . f)

initialLexSelection :: Sem -> Lexicon -> Macros -> ([TagElem], [ILexEntry], [[LexCombineError]])
initialLexSelection tsem lexicon grammar =
  (concat cands, lexCands, errs)
 where
  (errs, cands) = unzip $ map (combineList tsem grammar) lexCands
  lexCands      = chooseLexCand lexicon tsem

finaliseLexSelection :: MorphFn -> Sem -> [LitConstr] -> [TagElem] -> [TagElem]
finaliseLexSelection morph tsem litConstrs =
  setTidnums . considerCoherency . considerHasSem . considerLc . considerMorph
 where
   -- attach any morphological information to the candidates
   considerMorph = attachMorph morph tsem
   -- filter out candidates which do not fulfill the trace constraints
   matchesLc t = all (`elem` myTrace) constrs
         where constrs = concat [ cs | (l,cs) <- litConstrs, l `elem` mySem ]
               mySem   = tsemantics t
               myTrace = ttrace t
   considerLc = filter matchesLc
   -- filter out candidates whose semantics has bonus stuff which does
   -- not occur in the input semantics
   considerCoherency = filter (all (`elem` tsem) . tsemantics)
   considerHasSem    = filter (not . null . tsemantics)
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
newtype PreAnchoredL = PreAnchoredL [TagElem]

instance Loadable PreAnchoredL where
  lParse   = fmap PreAnchoredL
           . runParser geniTagElems () ""
  lSet _ p = p -- this does not update prog state at all
  lSummarise (PreAnchoredL ts) = show (length ts) ++ " trees"

readPreAnchored :: ProgStateRef -> IO [TagElem]
readPreAnchored pstRef = do
  PreAnchoredL ts <- loadOrDie (L :: L PreAnchoredL)
                        MacrosFlg "preanchored trees" pstRef
  return ts
\end{code}

% --------------------------------------------------------------------
% Boring utility code
% --------------------------------------------------------------------

\ignore{
\begin{code}
verbosity :: ProgStateRef -> IO Bool
verbosity = fmap (hasFlagP VerboseModeFlg . pa)
          . readIORef

instance JSON GeniResult where
 readJSON j =
    do jo <- fromJSObject `fmap` readJSON j
       let field x = maybe (fail $ "Could not find: " ++ x) readJSON
                   $ lookup x jo
       GeniResult <$> field "raw"
                  <*> field "realisations"
                  <*> field "derivation"
                  <*> field "lexical-selection"
                  <*> field "ranking"
                  <*> field "violations"
                  <*> field "result-type"
                  <*> field "chart-item"
 showJSON nr =
     JSObject . toJSObject $ [ ("raw", showJSON $ grLemmaSentence nr)
                             , ("realisations", showJSONs $ grRealisations nr)
                             , ("derivation", showJSONs $ grDerivation nr)
                             , ("lexical-selection", showJSONs $ grLexSelection nr)
                             , ("ranking", showJSON $ grRanking nr)
                             , ("violations", showJSONs $ grViolations nr)
                             , ("result-type", showJSON $ grResultType nr)
                             , ("chart-item", showJSON $ grOrigin nr)
                             ]

instance JSON ResultType where
  readJSON j =
    do js <- fromJSString `fmap` readJSON j
       case js of
         "partial"   -> return PartialResult
         "complete"  -> return CompleteResult
         ty          -> fail $ "unknown result type: " ++ ty
  showJSON CompleteResult = JSString $ toJSString "complete"
  showJSON PartialResult  = JSString $ toJSString "partial"

instance JSON GeniLexSel where
 readJSON j =
    do jo <- fromJSObject `fmap` readJSON j
       let field x = maybe (fail $ "Could not find: " ++ x) readJSON
                   $ lookup x jo
       GeniLexSel <$> field "lex-item"
                  <*> field "trace"
 showJSON x =
     JSObject . toJSObject $ [ ("lex-item", showJSON  $ nlTree x)
                             , ("trace",    showJSONs $ nlTrace x)
                             ]

-- Converts picoseconds to milliseconds.
picosToMillis :: Integer -> Double
picosToMillis t = realToFrac t / (10^(9 :: Int))

{-!
deriving instance NFData GeniResult
deriving instance NFData ResultType
deriving instance NFData GeniLexSel
!-}

-- GENERATED START

 
instance NFData GeniResult where
        rnf (GeniResult x1 x2 x3 x4 x5 x6 x7 x8)
          = rnf x1 `seq`
              rnf x2 `seq`
                rnf x3 `seq`
                  rnf x4 `seq` rnf x5 `seq` rnf x6 `seq` rnf x7 `seq` rnf x8 `seq` ()

 
instance NFData ResultType where
        rnf CompleteResult = ()
        rnf PartialResult = ()

 
instance NFData GeniLexSel where
        rnf (GeniLexSel x1 x2) = rnf x1 `seq` rnf x2 `seq` ()
-- GENERATED STOP
\end{code}
}
