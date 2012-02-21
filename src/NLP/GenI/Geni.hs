-- GenI surface realiser
-- Copyright (C) 2005 Carlos Areces and Eric Kow
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Geni is the interface between the front and backends of the generator. The GUI
--   and the console interface both talk to this module, and in turn, this module
--   talks to the input file parsers and the surface realisation engine.
module NLP.GenI.Geni (
             -- * Main interface

             -- ** Program state and configuration
             ProgState(..), ProgStateRef, emptyProgState,
             ProgStateLocal(..), resetLocal,
             LexicalSelector,

             -- ** Running GenI
             runGeni,
             GeniResult(..), isSuccess, GeniError(..), GeniSuccess(..),
             GeniLexSel(..),
             ResultType(..),

             -- * Helpers
             initGeni,
             lemmaSentenceString, prettyResult,
             showRealisations, histogram,
             getTraces,

             -- ** Loading things
             loadEverything,
             Loadable(..),
             loadLexicon,
             loadGeniMacros,
             loadTestSuite, loadTargetSemStr,
             loadRanking, BadInputException(..),
             loadFromString,
             )
where

import Control.Applicative ((<$>),(<*>))
import Control.Monad.Error
import Control.Exception

import Data.Binary (Binary, decodeFile)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.FullList ( fromFL )
import Data.List
import Data.List.Split ( wordsBy )
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)

import System.CPUTime( getCPUTime )
-- import System.Log.Logger
import NLP.GenI.Semantics ( sortByAmbiguity, Literal )
import NLP.GenI.Statistics
import Control.DeepSeq

import qualified System.IO.UTF8 as UTF8

import Text.JSON
-- import System.Process 

import NLP.GenI.General(
    histogram,
    geniBug,
    snd3,
    ePutStr, ePutStrLn, eFlush,
    -- mkLogname,
    )

import NLP.GenI.Btypes
  (Macros, ILexEntry, Lexicon,
   SemInput, Sem, LitConstr, TestCase(..), sortSem, removeConstraints,
   isemantics, iword,
   showLexeme,
   pidname, ptrace,
   )
import NLP.GenI.Tags (TagElem,
             idname,
             tsemantics,
             ttrace,
             setTidnums) 

import NLP.GenI.Configuration
  ( Params, customMorph
  , getFlagP, hasFlagP, hasOpt, Optimisation(NoConstraints)
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
                    runParser,
                    ParseError,
                    )
import NLP.GenI.GeniVal ( finaliseVars )
import NLP.GenI.LexicalSelection ( LexicalSelector, LexicalSelection(..), defaultLexicalSelector )
import NLP.GenI.Morphology
import NLP.GenI.OptimalityTheory
import NLP.GenI.Warnings

-- import CkyBuilder 
-- import SimpleBuilder (simpleBuilder)

-- -- DEBUG
-- import Control.Monad.Writer
-- import NLP.GenI.Lexicon
-- import NLP.GenI.LexicalSelection
-- import NLP.GenI.FeatureStructures

-- --------------------------------------------------------------------
-- ProgState
-- --------------------------------------------------------------------

data ProgState = ST{ -- | the current configuration being processed
                    pa     :: Params,
                    -- | loaded tree schemata
                    gr       :: Macros,
                    -- | loaded lexical entries
                    le       :: Lexicon,
                    -- | function to extract morphological information from
                    --   the semantics
                    --   (you may instead be looking for 'NLP.GenI.Configuration.customMorph')
                    morphinf :: MorphInputFn,
                    -- | lexical selection function (if you set this
                    --   you may want to add 'PreAnchored' to the config)
                    selector :: LexicalSelector,
                    -- | names of test case to run
                    tcase    :: String, 
                    -- | name, original string (for gui), sem
                    tsuite   :: [TestCase],
                    -- | OT constraints (optional)
                    ranking  :: OtRanking,
                    -- | simplified traces (optional)
                    traces   :: [String],
                    -- | see 'ProgStateLocal'
                    local    :: ProgStateLocal
               }

-- | The “local” program state is specific to a single input semantics.
--
--   The idea is that if we run GenI over a batch of inputs, we want to
--   be able to hit a reset button (see `resetLocal') between each run.
--
--   Better yet would be a more functional style that avoids all this!
data ProgStateLocal = STLocal {

    ts       :: SemInput
      -- | any warnings accumulated during realisation
      --   (most recent first)
  , warnings :: [GeniWarning]
}

emptyLocal :: ProgStateLocal
emptyLocal = STLocal
  { ts = ([],[],[])
  , warnings = []
  }

resetLocal :: SemInput -> ProgState -> ProgState
resetLocal sem p = p { local = emptyLocal { ts = sem } }

type ProgStateRef = IORef ProgState

-- | The program state when you start GenI for the very first time
emptyProgState :: Params -> ProgState
emptyProgState args =
 ST { pa = args
    , gr = []
    , le = []
    , morphinf = const Nothing
    , selector = \m l s -> return (defaultLexicalSelector m l s)
    , tcase = []
    , tsuite = []
    , traces = []
    , ranking = []
    , local   = emptyLocal
    }

-- | Log another warning in our internal program state
addWarning :: ProgStateRef -> GeniWarning -> IO ()
addWarning pstRef s = do
  -- warningM logname s
  modifyIORef pstRef $ \p -> p { local = tweak (local p) }
 where
  tweak l = l { warnings = s `appendWarning` warnings l }

-- --------------------------------------------------------------------
-- Interface
-- Loading and parsing
-- --------------------------------------------------------------------

-- | We have one master function that loads all the files GenI is expected to
--   use.  This just calls the sub-loaders below, some of which are exported
--   for use by the graphical interface.  The master function also makes sure
--   to complain intelligently if some of the required files are missing.
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


-- | The file loading functions all work the same way: we load the file,
--   and try to parse it.  If this doesn't work, we just fail in IO, and
--   GenI dies.  If we succeed, we update the program state passed in as
--   an IORef.
class Loadable x where
  lParse       :: FilePath -- ^ source (optional)
               -> String -> Either ParseError x
  lSet         :: x -> ProgState -> ProgState
  lSummarise   :: x -> String

-- | Note that here we assume the input consists of UTF-8 encoded file
lParseFromFile :: Loadable x => FilePath -> IO (Either ParseError x)
lParseFromFile f = lParse f `fmap` UTF8.readFile f

-- | Returns the input too (convenient for type checking)
lSetState :: Loadable x => ProgStateRef -> x -> IO x
lSetState pstRef x = modifyIORef pstRef (lSet x) >> return x

-- to be phased out
throwOnParseError :: String -> Either ParseError x -> IO x
throwOnParseError descr (Left err) = throwIO (BadInputException descr err)
throwOnParseError _ (Right p)  = return p

data BadInputException = BadInputException String ParseError
  deriving (Show, Typeable)

instance Exception BadInputException

data L a = Loadable a => L

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
     >>= throwOnParseError descr
     >>= lSetState pstRef
   return x

-- | Load something from a string rather than a file
loadFromString :: Loadable a => ProgStateRef
               -> String -- ^ description
               -> String -- ^ string to load
               -> IO a
loadFromString pstRef descr s =
  throwOnParseError descr (lParse "" s) >>= lSetState pstRef

instance Loadable Lexicon where
  lParse f = fmap toLexicon . runParser geniLexicon () f
    where
     fixEntry  = finaliseVars "" -- anonymise singletons for performance
               . sorter
     toLexicon = map fixEntry
     sorter l  = l { isemantics = (sortByAmbiguity . isemantics) l }
  lSet x p = p { le = x }
  lSummarise x = show (length x) ++ " lemmas"

instance Loadable Macros where
  lParse f = runParser geniMacros () f
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
     >>= throwOnParseError "tree schemata"
     >>= lSetState pstRef
  where
   descr = "trees"

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
     >>= throwOnParseError descr
     >>= lSetState pstRef
   let _ = x :: a
   return () -- ignore

newtype MorphFnL = MorphFnL MorphInputFn

instance Loadable MorphFnL where
  lParse f = fmap (MorphFnL . readMorph) . runParser geniMorphInfo () f
  lSet (MorphFnL x) p = p { morphinf = x }
  lSummarise _ = "morphinfo"

newtype TracesL = TracesL [String]

instance Loadable TracesL where
 lParse _ = Right . TracesL . lines
 lSet (TracesL xs) p = p { traces = xs }
 lSummarise (TracesL xs) = show (length xs) ++ " traces"

instance Loadable OtRanking where
  lParse _ = resultToEither2 . decode
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

-- Target semantics
--
-- Reading in the target semantics (or test suite) is a little more
-- complicated.  It follows the same general schema as above, except
-- that we parse the file twice: once for our internal representation,
-- and once to get a string representation of each test case.  The
-- string representation is for the graphical interface; it avoids us
-- figuring out how to pretty-print things because we can assume the
-- user will format it the way s/he wants.

newtype TestSuiteL = TestSuiteL [TestCase]

instance Loadable TestSuiteL where
 lParse f s =
   case runParser geniTestSuite () f s of
     Left e     -> Left e
     Right sem  -> case runParser geniTestSuiteString () f s of
        Left e      -> Left e
        Right mStrs -> Right (TestSuiteL (zipWith cleanup sem mStrs))
   where
    cleanup tc str =
        tc { tcSem = first3 sortSem (tcSem tc)
           , tcSemString = str }
    first3 g (x, y, z) = (g x, y, z)
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

-- Sometimes, the target semantics does not come from a file, but from
-- the graphical interface, so we also provide the ability to parse an
-- arbitrary string as the semantics.
newtype SemL = SemL SemInput

instance Loadable SemL where
 lParse f = fmap (SemL . smooth)
          . runParser geniSemanticInput () f
   where
    smooth (s,r,l) = (sortSem s, sort r, l)
 lSet (SemL x) p = resetLocal x p
 lSummarise (SemL _) = "sem input"

loadTargetSemStr :: ProgStateRef -> String -> IO ()
loadTargetSemStr pstRef s = do
  x <- loadFromString pstRef "semantics" s
  let _ = x :: SemL
  return ()

-- Helpers for loading files

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

-- --------------------------------------------------------------------
-- Surface realisation - entry point
-- --------------------------------------------------------------------

data GeniResult = GError   GeniError
                | GSuccess GeniSuccess
  deriving (Ord, Eq)

isSuccess :: GeniResult -> Bool
isSuccess (GSuccess _) = True
isSuccess (GError _)   = False

data GeniError = GeniError [String]
  deriving (Ord, Eq)

data GeniSuccess = GeniSuccess
 { grLemmaSentence     :: LemmaPlusSentence
 , grRealisations :: [String]
 , grDerivation   :: B.TagDerivation --type definition changed in Builder.hs 
 , grLexSelection :: [ GeniLexSel ]
 , grRanking      :: Int
 , grViolations   :: [ OtViolation ]
 , grResultType   :: ResultType
 , grOrigin       :: Integer -- normally a chart item id
 }
 deriving (Ord, Eq)

data GeniLexSel = GeniLexSel
 { nlTree  :: String
 , nlTrace :: [String]
 } deriving (Ord, Eq)

data ResultType = CompleteResult | PartialResult deriving (Ord, Eq)


instance Show GeniError where
  show (GeniError xs) = intercalate "\n" $ map ("Error: " ++) xs

-- | The entry point!
-- 
--   * Initialises the realiser (lexical selection, among other things),
--
--   * Runs the builder (the surface realisation engine proper)
--
--   * Unpacks the builder results 
--
--   * Finalises the results (morphological generation)
--
--   Returns a list of sentences, a set of Statistics, and the generator state.
--   The generator state is mostly useful for debugging via the graphical interface.
--   Note that we assumes that you have already loaded in your grammar and
--   parsed your input semantics.
runGeni :: ProgStateRef -> B.Builder st it Params -> IO ([GeniResult], Statistics, st)
runGeni pstRef builder = do
     modifyIORef pstRef $ \p -> resetLocal (ts (local p)) p
     pst <- readIORef pstRef
     let config = pa pst
         run    = B.run builder
         unpack = B.unpack builder
         finished = B.finished builder
     -- step 1: lexical selection
     initStuff <- initGeni pstRef
     start <- ( rnf initStuff ) `seq` getCPUTime  --force evaluation before measuring start time to avoid including grammar/lexicon parsing.

     -- step 2: chart generation
     let (finalSt, stats) = run initStuff config
     -- step 3: unpacking
     let uninflected = unpack finalSt
         tryPartial  = null uninflected && hasFlagP PartialFlg config
         rawResults  = if tryPartial then B.partial builder finalSt else uninflected
         resultTy    = if tryPartial then PartialResult else CompleteResult
         status      = finished finalSt
     -- step 4: post-processing
     results <- finaliseResults pstRef (resultTy, status, rawResults)
     end <-  ( rnf results ) `seq` getCPUTime --force evaluation before measuring end time to account for all the work that should be done.
     let elapsedTime = picosToMillis $! end - start
     let diff = round (elapsedTime :: Double) :: Int
     let stats2 = updateMetrics (incrIntMetric "gen_time"  (fromIntegral diff) ) stats

     return (results, stats2, finalSt)

-- --------------------------------------------------------------------
-- Surface realisation - sub steps
-- --------------------------------------------------------------------

-- | 'initGeni' performs lexical selection and strips the input semantics of
--   any morpohological literals
initGeni :: ProgStateRef -> IO (B.Input)
initGeni pstRef =
 do -- disable constraints if the NoConstraintsFlg pessimisation is active
    hasConstraints <- (hasOpt NoConstraints . pa) `fmap` readIORef pstRef
    when hasConstraints $
      modifyIORef pstRef $ \p -> p { local = killConstraints (local p) }
    -- lexical selection
    pst <- readIORef pstRef
    let (tsem,tres,lc) = ts (local pst)
        tsem2          = stripMorphSem (morphinf pst) tsem
    selection <- runLexSelection pstRef
    mapM_ (addWarning pstRef) (lsWarnings selection)
    -- strip morphological predicates
    let initStuff = B.Input 
          { B.inSemInput = (tsem2, tres, lc)
          , B.inLex   = lsLexEntries selection
          , B.inCands = map (\c -> (c,-1)) (lsAnchored selection)
          }
    return initStuff 
 where
   killConstraints l = l { ts = removeConstraints (ts l) }


-- | 'finaliseResults' does any post-processing steps that we want to integrate
--   into mainline GenI.  So far, this consists of morphological realisation and
--   OT ranking
finaliseResults :: ProgStateRef -> (ResultType, B.GenStatus, [B.Output]) -> IO [GeniResult]
finaliseResults pstRef (ty, status, os) =
 do pst <- readIORef pstRef
    -- morph TODO: make this a bit safer
    mss <- case getFlagP MorphCmdFlg (pa pst) of
             Nothing  -> let morph = fromMaybe (map sansMorph) (customMorph (pa pst))
                         in  return (morph sentences)
             Just cmd -> map snd `fmap` inflectSentencesUsingCmd cmd sentences
    -- OT ranking
    let unranked = zipWith (sansRanking pst) os mss
        rank = rankResults (getTraces pst) grDerivation (ranking pst)
        successes = map addRanking (rank unranked)
        failures  = case status of
                      B.Error str -> [GeniError [str]]
                      B.Finished  -> []
                      B.Active    -> []
    mapM_ (addWarning pstRef) $ map (MorphWarning . moWarnings) mss
    return (map GError failures ++ map GSuccess successes)
 where
  sentences = map snd3 os
  sansRanking pst (i,l,d) rs = GeniSuccess
               { grLemmaSentence = l
               , grRealisations = moRealisations rs
               , grDerivation   = d
               , grLexSelection = map (\x -> GeniLexSel x (getTraces pst x)) (B.lexicalSelection d)
               , grRanking = -1
               , grViolations = []
               , grResultType = ty
               , grOrigin     = i
               }
  addRanking (i,res,vs) = res { grViolations = vs, grRanking = i }

-- --------------------------------------------------------------------
-- Displaying results
-- --------------------------------------------------------------------

-- | Show the sentences produced by the generator, in a relatively compact form
showRealisations :: [String] -> String
showRealisations [] = "(none)"
showRealisations xs = unlines . map sho . Map.toList . histogram $ xs
  where
   sho (x,1) = x
   sho (x,c) = x ++ " (" ++ show c ++ " instances)"

-- | No morphology! Pretend the lemma string is a sentence
lemmaSentenceString :: GeniSuccess -> String
lemmaSentenceString = unwords . map lpLemma . grLemmaSentence

prettyResult :: ProgState -> GeniSuccess -> String
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

-- --------------------------------------------------------------------
-- Lexical selection
-- --------------------------------------------------------------------

-- | Runs the lexical selection (be it the standard GenI version or
--   a custom function supplied by a user) and runs the results
--   through the universal 'finaliseLexSelection'.
--
--   Also hunts for some warning conditions
runLexSelection :: ProgStateRef -> IO LexicalSelection
runLexSelection pstRef =
 do pst <- readIORef pstRef
    let (tsem,_,litConstrs) = ts (local pst)
        config   = pa pst
        verbose  = hasFlagP VerboseModeFlg config
    -- perform lexical selection
    selection <- (selector pst) (gr pst) (le pst) tsem
    let lexCand   = lsLexEntries selection
        candFinal = finaliseLexSelection (morphinf pst) tsem litConstrs (lsAnchored selection)
    -- status
    when verbose $
      do ePutStrLn $ "Lexical items selected:\n" ++ unlinesIndentAnd (showLexeme . fromFL . iword) lexCand
         ePutStrLn $ "Trees anchored (family) :\n" ++ unlinesIndentAnd idname candFinal
    -- warnings
    let semWarnings = case missingLiterals candFinal tsem of
                       [] -> []
                       xs -> [NoLexSelection xs]
    return $ selection { lsAnchored = candFinal
                       , lsWarnings = concat [ semWarnings, lsWarnings selection ]
                       }
 where
   indent  x = ' ' : x
   unlinesIndentAnd :: (x -> String) -> [x] -> String
   unlinesIndentAnd f = unlines . map (indent . f)

-- | @missingLiterals ts sem@ returns any literals in @sem@ that do not
--   appear in any of the @ts@ trees
missingLiterals :: [TagElem] -> [Literal] -> [Literal]
missingLiterals cands tsem =
   tsem \\ (nub $ concatMap tsemantics cands)

-- | Post-processes lexical selection results to things which
--   GenI considers applicable to all situations:
--
--   * attaches morphological information to trees
--
--   * throws out elementary trees that violate trace constraints
--     given by the user
--
--   * filters out any elementary tree whose semantics contains
--     things that are not in the input semantics
finaliseLexSelection :: MorphInputFn -> Sem -> [LitConstr] -> [TagElem] -> [TagElem]
finaliseLexSelection morph tsem litConstrs =
  setTidnums . considerCoherency . considerLc . considerMorph
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

-- --------------------------------------------------------------------
-- Pre-selection and pre-anchoring
-- --------------------------------------------------------------------

newtype PreAnchoredL = PreAnchoredL [TagElem]

instance Loadable PreAnchoredL where
  lParse f = fmap PreAnchoredL
           . runParser geniTagElems () f
  lSet _ p = p -- this does not update prog state at all
  lSummarise (PreAnchoredL xs) = show (length xs) ++ " trees"

readPreAnchored :: ProgStateRef -> IO [TagElem]
readPreAnchored pstRef = do
  PreAnchoredL xs <- loadOrDie (L :: L PreAnchoredL)
                        MacrosFlg "preanchored trees" pstRef
  return xs

-- --------------------------------------------------------------------
-- Boring utility code
-- --------------------------------------------------------------------

verbosity :: ProgStateRef -> IO Bool
verbosity = fmap (hasFlagP VerboseModeFlg . pa)
          . readIORef

instance JSON GeniResult where
 readJSON j =
    case readJSON j of
      Ok s    -> Ok (GSuccess s)
      Error _ -> GError `fmap` readJSON j
 showJSON (GSuccess x) = showJSON x
 showJSON (GError   x) = showJSON x

instance JSON GeniSuccess where
 readJSON j = do
   jo <- fromJSObject `fmap` readJSON j
   let field x = maybe (fail $ "Could not find: " ++ x) readJSON
               $ lookup x jo
   GeniSuccess <$> field "raw"
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

instance JSON GeniError where
 readJSON j =
    do jo <- fromJSObject `fmap` readJSON j
       let field x = maybe (fail $ "Could not find: " ++ x) readJSON
                   $ lookup x jo
       GeniError  <$> field "errors"
 showJSON (GeniError xs) =
     JSObject . toJSObject $ [ ("errors", showJSON xs) ]

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

{-
data MNAME = MNAME deriving Typeable
logname :: String
logname = mkLogname MNAME
-}

{-!
deriving instance NFData GeniResult
deriving instance NFData GeniSuccess
deriving instance NFData GeniError
deriving instance NFData ResultType
deriving instance NFData GeniLexSel
!-}

-- GENERATED START

 
instance NFData GeniResult where
        rnf (GError x1) = rnf x1 `seq` ()
        rnf (GSuccess x1) = rnf x1 `seq` ()

 
instance NFData GeniSuccess where
        rnf (GeniSuccess x1 x2 x3 x4 x5 x6 x7 x8)
          = rnf x1 `seq`
              rnf x2 `seq`
                rnf x3 `seq`
                  rnf x4 `seq` rnf x5 `seq` rnf x6 `seq` rnf x7 `seq` rnf x8 `seq` ()

 
instance NFData GeniError where
        rnf (GeniError x1) = rnf x1 `seq` ()

 
instance NFData ResultType where
        rnf (CompleteResult) = ()
        rnf (PartialResult) = ()

 
instance NFData GeniLexSel where
        rnf (GeniLexSel x1 x2) = rnf x1 `seq` rnf x2 `seq` ()
-- GENERATED STOP
