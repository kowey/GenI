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

{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeSynonymInstances      #-}

-- | This is the interface between the front and backends of the generator. The GUI
--   and the console interface both talk to this module, and in turn, this module
--   talks to the input file parsers and the surface realisation engine.
module NLP.GenI (
             -- * Main interface

             -- ** Program state and configuration
             ProgState(..), ProgStateRef, emptyProgState,
             LexicalSelector,

             -- ** Running GenI
             runGeni, simplifyResults, defaultCustomSem,
             GeniResults(..),
             GeniResult(..), isSuccess, GeniError(..), GeniSuccess(..),
             GeniLexSel(..),
             ResultType(..),

             -- * Helpers
             initGeni, extractResults,
             lemmaSentenceString, prettyResult,
             showRealisations, histogram,
             getTraces,

             -- ** Loading things
             loadEverything,
             Loadable(..),
             loadLexicon,
             loadGeniMacros,
             loadTestSuite, parseSemInput,
             loadRanking, BadInputException(..),
             loadFromString,
             )
where

import           Control.Applicative       ((<$>), (<*>))
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.Error
import           Data.Binary               (Binary, decodeFile)
import qualified Data.ByteString           as BS
import           Data.IORef                (IORef, modifyIORef, readIORef)
import           Data.List
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               (mempty, (<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.IO              as T
import           System.CPUTime            (getCPUTime)
import           System.FilePath           (takeExtension)
import           System.IO                 (stderr)

import           Data.FullList             (fromFL)
import           System.Log.Logger         (debugM)
import           Text.JSON

import qualified NLP.GenI.Builder          as B
import           NLP.GenI.Configuration
import           NLP.GenI.ErrorIO
import           NLP.GenI.General          (eFlush, ePutStr, ePutStrLn, first3,
                                            geniBug, histogram, mkLogname, snd3)
import           NLP.GenI.GeniShow
import           NLP.GenI.GeniVal
import           NLP.GenI.LexicalSelection (CustomSem (..),
                                            LexicalSelection (..),
                                            LexicalSelector,
                                            defaultLexicalSelector)
import           NLP.GenI.Lexicon
import           NLP.GenI.Morphology
import           NLP.GenI.OptimalityTheory
import           NLP.GenI.Parser           (ParseError, geniLexicon, geniMacros,
                                            geniMorphInfo, geniSemanticInput,
                                            geniTagElems, geniTestSuite,
                                            geniTestSuiteString, runParser)
import           NLP.GenI.Pretty           hiding ((<>))
import           NLP.GenI.Semantics
import           NLP.GenI.Statistics
import           NLP.GenI.Tag              (TagElem, idname, setTidnums,
                                            tsemantics)
import           NLP.GenI.TestSuite        (TestCase (..))
import           NLP.GenI.TreeSchema
import           NLP.GenI.Warning

-- -- DEBUG
-- import Control.Monad.Writer
-- import NLP.GenI.Lexicon
-- import NLP.GenI.LexicalSelection
-- import NLP.GenI.FeatureStructures

-- --------------------------------------------------------------------
-- ProgState
-- --------------------------------------------------------------------

-- | The program state consists of its configuration options and abstract,
--   cleaned up representations of all the data it's had to load into memory
--   (tree schemata files, lexicon files, etc).  The intention is for the
--   state to stay static until the next time something triggers some file
--   loading.
data ProgState = ProgState
    { pa       :: Params  -- ^ the current configuration
    , gr       :: Macros  -- ^ tree schemata
    , le       :: Lexicon -- ^ lexical entries
    , morphinf :: MorphInputFn -- ^ function to extract morphological
                               -- information from the semantics (you may
                               -- instead be looking for
                               -- 'NLP.GenI.Configuration.customMorph')
    , traces   :: [Text]    -- ^ simplified traces (optional)
    , customMorph :: Maybe MorphRealiser
    }

type ProgStateRef = IORef ProgState

-- | The program state when you start GenI for the very first time
emptyProgState :: Params -> ProgState
emptyProgState args = ProgState
    { pa = args
    , gr = []
    , le = []
    , morphinf = const Nothing
    , customMorph = Nothing
    , traces = []
    }

-- --------------------------------------------------------------------
-- Interface
-- Loading and parsing
-- --------------------------------------------------------------------

-- | We have one master function that loads all the files GenI is expected to
--   use.  This just calls the sub-loaders below, some of which are exported
--   for use by the graphical interface.  The master function also makes sure
--   to complain intelligently if some of the required files are missing.
loadEverything :: ProgStateRef -> CustomSem sem -> IO()
loadEverything pstRef wrangler = do
    pst <- readIORef pstRef
    --
    let config   = pa pst
        isMissing f = not $ hasFlagP f config
    -- grammar type
        grammarType      = getGrammarType (geniFlags config)
        isNotPreanchored = grammarType /= PreAnchored
        isNotPrecompiled = grammarType /= PreCompiled
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
    case grammarType of
       PreAnchored -> return ()
       PreCompiled -> return ()
       _           -> loadGeniMacros pstRef >> return ()
    -- we don't have to read in the lexicon if it's already pre-anchored
    when isNotPreanchored $ loadLexicon pstRef >> return ()
    -- in any case, we have to...
    loadMorphInfo pstRef
    when useTestSuite $ loadTestSuite pst wrangler >> return ()
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
                 -> Text -> Either Text x
    lSet         :: x -> ProgState -> ProgState
    lSummarise   :: x -> String

-- | Note that here we assume the input consists of UTF-8 encoded file
lParseFromFile :: Loadable x => FilePath -> IO (Either Text x)
lParseFromFile f = lParse f . T.decodeUtf8 <$> BS.readFile f

-- | Returns the input too (convenient for type checking)
lSetState :: Loadable x => ProgStateRef -> x -> IO x
lSetState pstRef x = modifyIORef pstRef (lSet x) >> return x

-- to be phased out
throwOnParseError :: String -> Either Text x -> IO x
throwOnParseError descr (Left err) = throwIO (BadInputException descr err)
throwOnParseError _ (Right p)  = return p

data BadInputException = BadInputException String Text
  deriving (Show, Typeable)

instance Exception BadInputException

data L a = Loadable a => L

-- | Load something, exiting GenI if we have not been given the
--   appropriate flag
loadOrDie :: forall f a . (Eq f, Typeable f, Loadable a)
          => L a
          -> (FilePath -> f) -- ^ flag
          -> String
          -> ProgStateRef
          -> IO a
loadOrDie L flg descr pstRef = do
    pst <- readIORef pstRef
    withFlagOrDie flg pst descr $ \f -> do
        v <- verbosity pstRef
        x <- withLoadStatus v f descr lSummarise lParseFromFile
                 >>= throwOnParseError descr
                 >>= lSetState pstRef
        return x

-- | Load something from a string rather than a file
loadFromString :: Loadable a => ProgStateRef
               -> String -- ^ description
               -> Text   -- ^ string to load
               -> IO a
loadFromString pstRef descr s =
    throwOnParseError descr (lParse "" s) >>= lSetState pstRef

instance Loadable Lexicon where
    lParse f =
        fmap toLexicon . fromParsec . runParser geniLexicon () f
      where
        fixEntry  = finaliseVars ""
                  . anonymiseSingletons  -- anonymise singletons for performance
                  . sorter
        toLexicon = map fixEntry
        sorter l  = l { isemantics = (sortByAmbiguity . isemantics) l }
    lSet x p = p { le = x }
    lSummarise x = show (length x) ++ " lemmas"

instance Loadable Macros where
    lParse f = fromParsec . runParser geniMacros () f
    lSet x p = p { gr = x }
    lSummarise x = show (length x) ++ " schemata"

loadLexicon :: ProgStateRef -> IO Lexicon
loadLexicon = loadOrDie (L :: L Lexicon) LexiconFlg "lexicon"

-- | The macros are stored as a hashing function in the monad.
loadGeniMacros :: ProgStateRef -> IO Macros
loadGeniMacros pstRef = do
    pst <- readIORef pstRef
    withFlagOrDie MacrosFlg pst descr $ \f -> do
        v <- verbosity pstRef
        withLoadStatus v f descr lSummarise parse
            >>= throwOnParseError "tree schemata"
            >>= lSetState pstRef
  where
    descr = "trees"
    parse = parseFromFileMaybeBinary lParseFromFile

-- | Load something, but only if we are configured to do so
loadOptional :: forall f a . (Eq f, Typeable f, Loadable a)
             => L a
             -> (FilePath -> f) -- ^ flag
             -> String
             -> ProgStateRef
             -> IO ()
loadOptional L flg descr pstRef = do
    pst <- readIORef pstRef
    withFlagOrIgnore flg pst $ \f -> do
        v <- verbosity pstRef
        x <- withLoadStatus v f descr lSummarise lParseFromFile
                 >>= throwOnParseError descr
                 >>= lSetState pstRef
        let _ = x :: a
        return () -- ignore

newtype MorphFnL = MorphFnL MorphInputFn

instance Loadable MorphFnL where
    lParse f = fmap (MorphFnL . readMorph)
             . fromParsec
             . runParser geniMorphInfo () f
    lSet (MorphFnL x) p = p { morphinf = x }
    lSummarise _ = "morphinfo"

newtype TracesL = TracesL [Text]

instance Loadable TracesL where
    lParse _ = Right . TracesL . T.lines
    lSet (TracesL xs) p = p { traces = xs }
    lSummarise (TracesL xs) = show (length xs) ++ " traces"

instance Loadable OtRanking where
    lParse _ = resultToEither2 . decode . T.unpack
    lSet r p = p { pa = (pa p) { ranking = r } }
    lSummarise _ = "ranking"

loadMorphInfo :: ProgStateRef -> IO ()
loadMorphInfo = loadOptional (L :: L MorphFnL) MorphInfoFlg "morphological info"

loadTraces :: ProgStateRef -> IO ()
loadTraces = loadOptional (L :: L TracesL) TracesFlg "traces"

loadRanking :: ProgStateRef -> IO ()
loadRanking = loadOptional (L :: L OtRanking) RankingConstraintsFlg "OT constraints"

fromParsec :: Either ParseError a -> Either Text a
fromParsec (Left err) = Left . T.pack $ show err
fromParsec (Right a)  = Right a

resultToEither2 :: Result a -> Either Text a
resultToEither2 r =
    case resultToEither r of
        Left e  -> Left (T.pack e)
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

newtype TestSuiteL = TestSuiteL { fromTestSuiteL :: [TestCase SemInput] }

instance Loadable TestSuiteL where
    lParse f s = fromParsec $ do
        sem   <- runParser geniTestSuite () f s
        mStrs <- runParser geniTestSuiteString () f s
        return $ TestSuiteL (zipWith cleanup sem mStrs)
      where
        cleanup tc str = tc
            { tcSem = first3 sortSem (tcSem tc)
            , tcSemString = str
            }
    --
    lSet (TestSuiteL _) p = p
    lSummarise (TestSuiteL x) = show (length x) ++ " cases"

-- |
loadTestSuite :: ProgState -> CustomSem sem -> IO [TestCase sem]
loadTestSuite pst wrangler = do
    withFlagOrDie flg pst descr $ \f ->
         withLoadStatus v f descr summary pfile
         >>= throwOnParseError descr
  where
    v       = hasFlagP VerboseModeFlg (pa pst)
    pfile f = customSuiteParser wrangler f <$> readFileUtf8 f
    flg   = TestSuiteFlg
    descr = "test suite"
    summary xs = show (length xs) ++ " test cases"

parseSemInput :: Text -> Either ParseError SemInput
parseSemInput =
    fmap smooth . runParser geniSemanticInput () "semantics"
  where
    smooth (s,r,l) = (sortSem s, sort r, l)

-- Helpers for loading files

withFlag :: forall f a . (Eq f, Typeable f)
         => (FilePath -> f) -- ^ flag
         -> ProgState
         -> IO a               -- ^ null action
         -> (FilePath -> IO a) -- ^ job
         -> IO a
withFlag flag pst z job =
    maybe z job $ getFlagP flag (pa pst)

withFlagOrIgnore :: forall f . (Eq f, Typeable f)
                 => (FilePath -> f) -- ^ flag
                 -> ProgState
                 -> (FilePath -> IO ())
                 -> IO ()
withFlagOrIgnore flag pst = withFlag flag pst (return ())

withFlagOrDie :: forall f a . (Eq f, Typeable f)
              => (FilePath -> f) -- ^ flag
              -> ProgState
              -> String
              -> (FilePath -> IO a)
              -> IO a
withFlagOrDie flag pst description =
    withFlag flag pst (fail msg)
  where
    msg = "Please specify a " ++ description ++ " file!"

withLoadStatus :: Bool                 -- ^ verbose
               -> FilePath             -- ^ file to load
               -> String               -- ^ description
               -> (a -> String)        -- ^ summary
               -> (FilePath -> IO (Either Text a)) -- ^ parsing cmd
               -> IO (Either Text a)
withLoadStatus False f _ _         p = p f
withLoadStatus True  f d summarise p = do
    ePutStr $ unwords [ "Loading",  d, f ++ "... " ]
    eFlush
    mx <- p f
    ePutStrLn $ either (const "ERROR") (\x -> summarise x ++ " loaded") mx
    return mx

parseFromFileMaybeBinary :: Binary a
                         => (FilePath -> IO (Either Text a))
                         -> FilePath
                         -> IO (Either Text a)
parseFromFileMaybeBinary p f =
    if takeExtension f == ".genib"
       then Right <$> decodeFile f
       else p f

-- --------------------------------------------------------------------
-- Surface realisation - entry point
-- --------------------------------------------------------------------

-- | 'GeniResults' is the outcome of running GenI on a single input semantics.
--   Each distinct result is returned as a single 'GeniResult' (NB: a single
--   result may expand into multiple strings through morphological
--   post-processing),
data GeniResults = GeniResults
    { grResults        :: [GeniResult] -- ^ one per chart item
    , grGlobalWarnings :: [Text]       -- ^ usually from lexical selection
    , grStatistics     :: Statistics   -- ^ things like number of chart items
                                       --   to help study efficiency
    }

data GeniResult = GError   GeniError
                | GSuccess GeniSuccess
  deriving (Ord, Eq)

isSuccess :: GeniResult -> Bool
isSuccess (GSuccess _) = True
isSuccess (GError _)   = False

data GeniError = GeniError [Text]
  deriving (Ord, Eq)

data GeniSuccess = GeniSuccess
    { grLemmaSentence :: LemmaPlusSentence -- ^ “original” uninflected result
    , grRealisations  :: [Text]            -- ^ results after morphology
    , grResultType    :: ResultType
    , grWarnings      :: [Text]            -- ^ warnings “local” to this particular
                                           --   item, cf. 'grGlobalWarnings'
    , grDerivation    :: B.TagDerivation   -- ^ derivation tree behind the result
    , grOrigin        :: Integer           -- ^ normally a chart item id
    , grLexSelection  :: [GeniLexSel]      -- ^ the lexical selection behind
                                           --   this result (info only)
    , grRanking       :: Int               -- ^ see 'NLP.GenI.OptimalityTheory'
    , grViolations    :: [OtViolation]     -- ^ which OT constraints were violated
    } deriving (Ord, Eq)

data GeniLexSel = GeniLexSel
    { nlTree  :: Text
    , nlTrace :: [Text]
    } deriving (Ord, Eq)

data ResultType = CompleteResult | PartialResult deriving (Ord, Eq)

instance Pretty GeniError where
    pretty (GeniError xs) = T.intercalate "\n" $ map ("Error:" <+>) xs

-- | Entry point! (the most useful function to know here)
--
--   * Initialises the realiser (lexical selection, among other things),
--
--   * Runs the builder (the surface realisation engine proper)
--
--   * Unpacks the builder results
--
--   * Finalises the results (morphological generation)
--
--   In addition to the results, this returns a generator state.  The latter is
--   is mostly useful for debugging via the graphical interface.
--   Note that we assumes that you have already loaded in your grammar and
--   parsed your input semantics.
runGeni :: ProgState
        -> CustomSem sem
        -> B.Builder st it
        -> TestCase sem
        -> ErrorIO (GeniResults,st)
runGeni pst selector builder tc = do
    -- step 1: lexical selection
    istuff <- initGeni pst selector semInput
    -- steps 2 to 4
    liftIO $ runBuilder istuff
  where
    semInput = tcSem tc
    runBuilder (initStuff, initWarns) = do
        let flags  = geniFlags (pa pst)
            run    = B.run builder
        --force evaluation before measuring start time to avoid including grammar/lexicon parsing.
        start <- rnf initStuff `seq` getCPUTime
        -- step 2: chart generation
        let (finalSt, stats) = run initStuff flags
        -- step 3: unpacking and
        -- step 4: post-processing
        results <- extractResults pst builder finalSt
        --force evaluation before measuring end time to account for all the work that should be done.
        end <- rnf results `seq` getCPUTime
        let elapsedTime = picosToMillis $! end - start
            diff = round (elapsedTime :: Double) :: Int
            stats2 = updateMetrics (incrIntMetric "gen_time"  (fromIntegral diff) ) stats
            gresults = GeniResults { grResults        = results
                                   , grStatistics     = stats2
                                   , grGlobalWarnings = map showWarnings (fromGeniWarnings initWarns)
                                   }
        return (gresults, finalSt)
    showWarnings = T.intercalate "\n" . showGeniWarning

-- | @simplifyResults <$> runGenI...'@ for an easier time if you don't need the
--   surface realiser state
simplifyResults :: Either Text (GeniResults, st) -> GeniResults
simplifyResults (Left t) = GeniResults
    { grResults        = [GError $ GeniError [t]]
    , grGlobalWarnings = []
    , grStatistics     = emptyStats
    }
simplifyResults (Right (r,_)) = r

-- | This is a helper to 'runGenI'. It's mainly useful if you are building
--   interactive GenI debugging tools.
--
--   Given a builder state,
--
--   * Unpacks the builder results
--
--   * Finalises the results (morphological generation)
extractResults :: ProgState ->  B.Builder st it -> st -> IO [GeniResult]
extractResults pst builder finalSt = do
    let config = pa pst
    -- step 3: unpacking
    let uninflected = B.unpack builder finalSt
        (rawResults, resultTy) =
            if null uninflected && hasFlagP PartialFlg config
               then (B.partial builder finalSt, PartialResult)
               else (uninflected              , CompleteResult)
        status = B.finished builder finalSt
    -- step 4: post-processing
    debugM logname $ "tree assembly status: " ++ prettyStr status
    finaliseResults pst (resultTy, status, rawResults)

-- --------------------------------------------------------------------
-- Surface realisation - sub steps
-- --------------------------------------------------------------------

-- | 'initGeni' performs lexical selection and strips the input semantics of
--   any morpohological literals
--
--   See 'defaultCustomSem'
initGeni :: ProgState
         -> CustomSem sem
         -> sem
         -> ErrorIO (B.Input, GeniWarnings)
initGeni pst wrangler csem = do
    -- lexical selection
    selection <- runLexSelection pst wrangler csem
    liftIO $ debugM logname $
        "lexical selection returned " ++
        (show . length $ lsAnchored selection) ++
        " anchored trees"
    semInput <- liftEither $ fromCustomSemInput wrangler csem
    let initStuff = B.Input
          { B.inSemInput = semInput
          , B.inLex   = lsLexEntries selection
          , B.inCands = map (\c -> (c,-1)) (lsAnchored selection)
          }
    return (initStuff, lsWarnings selection)

-- | 'finaliseResults' does any post-processing steps that we want to integrate
--   into mainline GenI.  So far, this consists of morphological realisation and
--   OT ranking
finaliseResults :: ProgState -> (ResultType, B.GenStatus, [B.Output]) -> IO [GeniResult]
finaliseResults pst (ty, status, os) = do
    debugM logname $ "finalising " ++ show (length sentences) ++ " results"
    -- morph TODO: make this a bit safer
    mss <- case getFlagP MorphCmdFlg (pa pst) of
             Nothing  -> let morph = fromMaybe (map sansMorph) (customMorph pst)
                         in  return (morph sentences)
             Just cmd -> map snd `fmap` inflectSentencesUsingCmd cmd sentences
    -- OT ranking
    let unranked = zipWith sansRanking os mss
        rank = rankResults (getTraces pst) grDerivation (ranking (pa pst))
        successes = map addRanking (rank unranked)
        failures  = case status of
                      B.Error str -> [GeniError [str]]
                      B.Finished  -> []
                      B.Active    -> []
    return (map GError failures ++ map GSuccess successes)
 where
  sentences = map snd3 os
  sansRanking (i,l,d) rs = GeniSuccess
               { grLemmaSentence = l
               , grRealisations = moRealisations rs
               , grWarnings     = moWarnings rs
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
lemmaSentenceString :: GeniSuccess -> Text
lemmaSentenceString = T.unwords . map lpLemma . grLemmaSentence

prettyResult :: ProgState -> GeniSuccess -> Text
prettyResult pst nr =
    T.intercalate "\n" . map showOne . grRealisations $ nr
  where
    showOne str = pretty theRanking <> ". " <> str <> "\n" <> violations
    violations  = prettyViolations tracesFn verbose (grViolations nr)
    theRanking  = grRanking nr
    verbose  = hasFlagP VerboseModeFlg (pa pst)
    tracesFn = getTraces pst

-- | 'getTraces' is most likely useful for grammars produced by a
--   metagrammar system.  Given a tree name, we retrieve the ``trace''
--   information from the grammar for all trees that have this name.  We
--   assume the tree name was constructed by GenI; see the source code for
--   details.
getTraces :: ProgState -> Text -> [Text]
getTraces pst tname =
    filt $ concat [ ptrace t | t <- gr pst, pidname t == readPidname tname ]
  where
    filt = case traces pst of
               []    -> id
               theTs -> filter (`elem` theTs)

-- | We assume the name was constructed by 'combineName'
readPidname :: Text -> Text
readPidname n =
    case T.splitOn ":" n of
        (_:_:p:_) -> p
        _         -> geniBug "NLP.GenI.readPidname or combineName are broken"

-- --------------------------------------------------------------------
-- Lexical selection
-- --------------------------------------------------------------------

-- | Runs the lexical selection (be it the standard GenI version or
--   a custom function supplied by a user) and runs the results
--   through the universal 'finaliseLexSelection'.
--
--   Also hunts for some warning conditions
runLexSelection :: ProgState
                -> CustomSem sem -- ^ handler for custom semantics
                -> sem           -- ^ semantics
                -> ErrorIO LexicalSelection
runLexSelection pst wrangler csem = do
    let config   = pa pst
        verbose  = hasFlagP VerboseModeFlg config
        selector = customSelector wrangler
    -- perform lexical selection
    selection <- liftIO $ selector (gr pst) (le pst) csem
    -- finalise selection
    sem@(tsem, _, _) <- liftEither $ fromCustomSemInput wrangler csem
    let lexCand   = lsLexEntries selection
        candFinal = finaliseLexSelection (morphinf pst) sem (lsAnchored selection)
    -- status
    when verbose $ liftIO $ T.hPutStrLn stderr . T.unlines $
        "Lexical items selected:"
        :  map (indent . showLexeme . fromFL . iword) lexCand
        ++ ["Trees anchored (family) :"]
        ++ map (indent . idname) candFinal
    -- warnings
    let semWarnings = case missingLiterals candFinal tsem of
                       [] -> []
                       xs -> [NoLexSelection xs]
    return $ selection { lsAnchored = candFinal
                       , lsWarnings = mkGeniWarnings semWarnings <> lsWarnings selection
                       }
  where
    indent  x = ' ' `T.cons` x

-- | Standard GenI semantics and lexical selection algorithm
--   (with optional "preanchored" mode)
defaultCustomSem :: ProgState -> IO (CustomSem SemInput)
defaultCustomSem pst = mkDefaultCustomSem pst <$>
    case grammarType of
        PreAnchored -> mkPreAnchoredLexicalSelector pst
        _           -> return defaultLexicalSelector
  where
    grammarType = getGrammarType $ geniFlags . pa $ pst

mkDefaultCustomSem :: ProgState
                    -> LexicalSelector SemInput
                    -> CustomSem SemInput
mkDefaultCustomSem pst selector = CustomSem
    { fromCustomSemInput = Right
    , customSelector     = \t l s -> selector t l (tweakSem s)
    , customRenderSem    = geniShowText
    , customSemParser    = \t ->
          trivialTestCase t <$> (fromParsec . parseSemInput) t
    , customSuiteParser  = \f -> fmap fromTestSuiteL . lParse f
    }
  where
    trivialTestCase t s = TestCase
        { tcName      = "(sem)"
        , tcSemString = t
        , tcSem       = s
        , tcExpected  = []
        , tcParams    = Nothing
        }
    tweakSem = stripMorphStuff . maybeRemoveConstraints
    stripMorphStuff = first3 (stripMorphSem (morphinf pst))
    -- disable constraints if the NoConstraintsFlg pessimisation is active
    maybeRemoveConstraints =
         if hasOpt NoConstraints (geniFlags (pa pst)) then removeConstraints else id

-- | @missingLiterals ts sem@ returns any literals in @sem@ that do not
--   appear in any of the @ts@ trees
missingLiterals :: [TagElem] -> [Literal GeniVal] -> [Literal GeniVal]
missingLiterals cands tsem =
    tsem \\ (nub $ concatMap tsemantics cands)

-- | Post-processes lexical selection results to things which
--   GenI considers universal. No matter what custom
--   lexical selection mechanism you supply, these preflight
--   checks will run.
--
--   * attaches morphological information to trees
--
--   * filters out any elementary tree whose semantics contains
--     things that are not in the input semantics
finaliseLexSelection :: MorphInputFn -> SemInput -> [TagElem] -> [TagElem]
finaliseLexSelection morph (tsem,_,_) =
    setTidnums . considerCoherency . considerMorph
  where
    -- attach any morphological information to the candidates
    considerMorph = attachMorph morph tsem
    -- filter out candidates whose semantics has bonus stuff which does
    -- not occur in the input semantics
    considerCoherency = filter (all (`elem` tsem) . tsemantics)

-- --------------------------------------------------------------------
-- Pre-selection and pre-anchoring
-- --------------------------------------------------------------------

newtype PreAnchoredL = PreAnchoredL [TagElem]

instance Loadable PreAnchoredL where
    lParse f = fmap PreAnchoredL
             . fromParsec
             . runParser geniTagElems () f
    lSet _ p = p -- this does not update prog state at all
    lSummarise (PreAnchoredL xs) = show (length xs) ++ " trees"

readPreAnchored :: ProgState -> IO [TagElem]
readPreAnchored pst = withFlagOrDie flg pst descr $ \f -> do
    x <- withLoadStatus v f descr lSummarise lParseFromFile
             >>= throwOnParseError descr
    let PreAnchoredL xs = x
    return xs
  where
    v     = hasFlagP VerboseModeFlg (pa pst)
    flg   = MacrosFlg
    descr = "preanchored trees"

mkPreAnchoredLexicalSelector :: ProgState -> IO (LexicalSelector SemInput)
mkPreAnchoredLexicalSelector pst = do
    xs <- readPreAnchored pst
    return (\_ _ _ -> return (LexicalSelection xs [] mempty))

-- --------------------------------------------------------------------
-- Boring utility code
-- --------------------------------------------------------------------

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 f = T.decodeUtf8 <$> BS.readFile f

verbosity :: ProgStateRef -> IO Bool
verbosity = fmap (hasFlagP VerboseModeFlg . pa)
          . readIORef

instance JSON GeniResults where
    readJSON j = do
        jo <- fromJSObject `fmap` readJSON j
        let field x = maybe (fail $ "Could not find: " ++ x) readJSON
                    $ lookup x jo
        GeniResults <$> field "results"
                    <*> field "warnings"
                    <*> field "statistics"
    showJSON x = JSObject . toJSObject $
        [ ("results",      showJSONs $ grResults x)
        , ("warnings",     showJSONs $ grGlobalWarnings x)
        , ("statistics",   showJSON  $ grStatistics x)
        ]

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
                    <*> field "result-type"
                    <*> field "warnings"
                    <*> field "derivation"
                    <*> field "chart-item"
                    <*> field "lexical-selection"
                    <*> field "ranking"
                    <*> field "violations"
    showJSON nr = JSObject . toJSObject $
        [ ("raw"              , showJSON  $ grLemmaSentence nr)
        , ("realisations"     , showJSONs $ grRealisations nr)
        , ("derivation"       , showJSONs $ grDerivation nr)
        , ("lexical-selection", showJSONs $ grLexSelection nr)
        , ("ranking"          , showJSON  $ grRanking nr)
        , ("violations"       , showJSONs $ grViolations nr)
        , ("result-type"      , showJSON  $ grResultType nr)
        , ("chart-item"       , showJSON  $ grOrigin nr)
        , ("warnings"         , showJSONs $ grWarnings nr)
        ]

instance JSON GeniError where
    readJSON j = do
        jo <- fromJSObject <$> readJSON j
        let field x = maybe (fail $ "Could not find: " ++ x) readJSON
                    $ lookup x jo
        GeniError  <$> field "errors"
    showJSON (GeniError xs) =
        JSObject . toJSObject $ [ ("errors", showJSON xs) ]

instance JSON ResultType where
    readJSON j = do
        js <- fromJSString `fmap` readJSON j
        case js of
            "partial"   -> return PartialResult
            "complete"  -> return CompleteResult
            ty          -> fail $ "unknown result type: " ++ ty
    showJSON CompleteResult = JSString $ toJSString "complete"
    showJSON PartialResult  = JSString $ toJSString "partial"

instance JSON GeniLexSel where
    readJSON j = do
        jo <- fromJSObject `fmap` readJSON j
        let field x = maybe (fail $ "Could not find: " ++ x) readJSON
                    $ lookup x jo
        GeniLexSel <$> field "lex-item"
                   <*> field "trace"
    showJSON x = JSObject . toJSObject $
        [ ("lex-item", showJSON  $ nlTree x)
        , ("trace",    showJSONs $ nlTrace x)
        ]

-- Converts picoseconds to milliseconds.
picosToMillis :: Integer -> Double
picosToMillis t = realToFrac t / (10^(9 :: Int))

data MNAME = MNAME deriving Typeable
logname :: String
logname = mkLogname MNAME

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
        rnf (GeniSuccess x1 x2 x3 x4 x5 x6 x7 x8 x9)
          = rnf x1 `seq`
              rnf x2 `seq`
                rnf x3 `seq`
                  rnf x4 `seq`
                    rnf x5 `seq` rnf x6 `seq` rnf x7 `seq` rnf x8 `seq` rnf x9 `seq` ()


instance NFData GeniError where
        rnf (GeniError x1) = rnf x1 `seq` ()


instance NFData ResultType where
        rnf CompleteResult = ()
        rnf PartialResult = ()


instance NFData GeniLexSel where
        rnf (GeniLexSel x1 x2) = rnf x1 `seq` rnf x2 `seq` ()
-- GENERATED STOP
