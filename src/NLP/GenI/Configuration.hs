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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module NLP.GenI.Configuration
    ( Params(..)
    --
    , mainBuilderTypes
    , getFlagP, getListFlagP, modifyFlagP, setFlagP, hasFlagP, deleteFlagP
    , emptyParams, defineParams
    , treatArgs, treatArgsWithParams, usage, basicSections, optionsSections
    , processInstructions
    , optionsForStandardGenI
    , optionsForBasicStuff, optionsForOptimisation, optionsForMorphology, optionsForInputFiles
    , optionsForBuilder, optionsForTesting
    , helpOption, verboseOption, macrosOption, lexiconOption
    , nubBySwitches
    , noArg, reqArg, optArg
    , parseFlagWithParsec
    -- * configration files
    , readGlobalConfig, setLoggers
    -- re-exports
    , module System.Console.GetOpt
    , module NLP.GenI.Flag
    , Typeable
    )
where

import Control.Applicative ( (<$>), pure )
import Control.Arrow ( first )
import Control.Monad ( liftM )
import Data.Char ( toLower, isSpace )
import Data.List  ( find, intersperse, nubBy )
import Data.Maybe ( fromMaybe, isNothing, fromJust )
import Data.Maybe ( listToMaybe, mapMaybe )
import Data.String ( IsString(..) )
import Data.Text ( Text )
import Data.Typeable ( Typeable )
import System.Directory ( getAppUserDataDirectory, doesFileExist )
import System.Environment ( getProgName )
import System.FilePath
import System.IO ( stderr )
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Yaml.YamlLight
import System.Console.GetOpt
import System.Log.Formatter
import System.Log.Handler ( LogHandler, setFormatter )
import System.Log.Handler.Simple
import System.Log.Logger

import NLP.GenI.Flag
import NLP.GenI.General ( geniBug, fst3, snd3 )
import NLP.GenI.Parser ( geniFeats, tillEof, Parser, runParser )
import NLP.GenI.Morphology.Types ( MorphRealiser )
import NLP.GenI.Pretty
import NLP.GenI.Polarity.Types ( readPolarityAttrs )

-- --------------------------------------------------------------------
-- Params
-- --------------------------------------------------------------------

-- | Holds the specification for how Geni should be run, its input
--   files, etc.  This is the stuff that would normally be found in
--   the configuration file.
data Params = Params
    { grammarType    :: GrammarType
    , builderType    :: BuilderType
    -- | Can still be overridden with a morph command mind you
    , customMorph    :: Maybe MorphRealiser
    , geniFlags      :: [Flag]
    }


{-
instance Show Params where
    show p = unlines
        [ unwords [ "GenI config :", show (grammarType p), show (builderType p), morph ]
        , unwords $ "GenI flags  :" : map show (geniFlags p)
        ]
      where
        morph = "custom morph:" ++ show (isJust (customMorph p))
-}

-- | The default parameters configuration
emptyParams :: Params
emptyParams = Params
    { builderType    = SimpleBuilder
    , grammarType    = GeniHand
    , customMorph    = Nothing
    , geniFlags      = emptyFlags
    }

hasFlagP    :: (Typeable f, Typeable x) => (x -> f) -> Params -> Bool
hasFlagP f      = hasFlag f . geniFlags

deleteFlagP :: (Typeable f, Typeable x) => (x -> f) -> Params -> Params
deleteFlagP f p = p { geniFlags = deleteFlag f (geniFlags p) }

modifyFlagP :: (Eq f, Typeable f, Typeable x)
            => (x -> f) -> (x -> x) -> Params -> Params
modifyFlagP f m p = p { geniFlags = modifyFlag f m (geniFlags p) }

setFlagP    :: (Eq f, Typeable f, Typeable x)
            => (x -> f) -> x -> Params -> Params
setFlagP f v p  = p { geniFlags = setFlag f v (geniFlags p) }

getFlagP    :: (Typeable f, Typeable x)
            => (x -> f) -> Params -> Maybe x
getFlagP f = getFlag f . geniFlags

getListFlagP :: (Typeable f, Typeable x)
             => ([x] -> f) -> Params -> [x]
getListFlagP f = fromMaybe [] . getFlagP f

emptyFlags :: [Flag]
emptyFlags =
    [ Flag ViewCmdFlg "ViewTAG"
    , Flag DetectPolaritiesFlg $
          readPolarityAttrs defaultPolarityAttrs
    , Flag RootFeatureFlg $
          parseFlagWithParsec "default root feat" geniFeats defaultRootFeat
    ]

-- --------------------------------------------------------------------
-- Command line arguments
-- --------------------------------------------------------------------

type OptSection = (String,[OptDescr Flag],[String])

-- | Uses the GetOpt library to process the command line arguments.
-- Note that we divide them into basic and advanced usage.
optionsForStandardGenI :: [OptDescr Flag]
optionsForStandardGenI =
    nubBySwitches $ concatMap snd3 optionsSections
        ++ [ Option ['p']    []  (reqArg WeirdFlg id "CMD") "" ]
        -- TODO: what is this -p flag for, exactly? It's something
        -- related to how GenI runs within an app bundle.  Can we
        -- do away with it?

basicSections :: [OptSection]
basicSections =
    map tweakBasic $ take 1 optionsSections
  where
    tweakBasic (x,y,z) = (x,y,z ++ ["See --help for more options"])

optionsSections :: [OptSection]
optionsSections =
    [ ("Core options", optionsForBasicStuff, example)
    , ("Input", optionsForInputFiles, [])
    , ("Output", optionsForOutput, [])
    , ("Algorithm",
        (nubBySwitches $ optionsForBuilder ++ optionsForOptimisation),
        usageForOptimisations)
    , ("Morphology", optionsForMorphology, [])
    , ("User interface", optionsForUserInterface, [])
    , ("Batch processing", optionsForTesting, [])
    ]
  where
      example =
          [ "Example:"
          , " geni -m examples/ej/mac -l examples/ej/lexicon -s examples/ej/suite"
          ]

getSwitches :: OptDescr a -> ([Char],[String])
getSwitches (Option s l _ _) = (s,l)

nubBySwitches :: [OptDescr a] -> [OptDescr a]
nubBySwitches = nubBy (\x y -> getSwitches x == getSwitches y)

-- GetOpt wrappers
noArg :: forall f . (Eq f, Typeable f)
      => (() -> f) -> ArgDescr Flag
noArg  s = NoArg (Flag s ())

reqArg :: forall f x . (Eq f, Typeable f, Eq x, Typeable x)
       => (x -> f)      -- ^ flag
       -> (String -> x) -- ^ string reader for flag (probably |id| if already a String)
       -> String        -- ^ description
       -> ArgDescr Flag
reqArg s fn desc = ReqArg (\x -> Flag s (fn x)) desc

optArg :: forall f x . (Eq f, Typeable f, Eq x, Typeable x)
       => (x -> f)       -- ^ flag
       -> x              -- ^ default value
       -> (String -> x)  -- ^ string reader (as in @reqArg@)
       -> String         -- ^ description
       -> ArgDescr Flag
optArg s def fn desc = OptArg (\x -> Flag s (maybe def fn x)) desc

-- -------------------------------------------------------------------
-- Parsing command line arguments
-- -------------------------------------------------------------------

-- | Print out a GenI-style usage message with options divided into sections
usage :: [OptSection] -- ^ options
      -> String -- ^ prog name
      -> String
usage sections pname =
 let header   = "Usage: " ++ pname ++ " [OPTION...]\n"
     body     = unlines $ map usageSection sections
 in header ++ body

usageSection :: (String, [OptDescr Flag],[String]) -> String
usageSection (name, opts, comments) =
 usageInfo (unlines $ [bar,name, bar]) opts ++ mcomments
 where
  bar = replicate 72 '='
  mcomments = if null comments then [] else "\n" ++ unlines comments

treatArgs :: [OptDescr Flag] -> [String] -> IO Params
treatArgs options argv = treatArgsWithParams options argv emptyParams

treatArgsWithParams :: [OptDescr Flag] -> [String] -> Params -> IO Params
treatArgsWithParams options argv initParams =
   case getOpt Permute options argv of
     (os,_,[]  )-> return $ defineParams os initParams
     (_,_,errs) -> do p <- getProgName
                      ioError (userError $ concat errs ++ usage basicSections p)

defineParams :: [Flag] -> Params -> Params
defineParams flgs prms =
  (\p -> foldr setDefault p $ geniFlags prms)
  . (mergeFlagsP OptimisationsFlg)
  . (mergeFlagsP MetricsFlg)
  $ prms
    { geniFlags     = flgs
    , builderType   = fromFlags builderType BuilderFlg flgs
    , grammarType   = fromFlags grammarType GrammarTypeFlg flgs
    }
 where
  setDefault (Flag f v) p =
    if hasFlagP f p then p else setFlagP f v p
  mergeFlagsP f p =
    if hasFlagP f p
    then setFlagP f (concat $ getAllFlags f flgs) p
    else p
  fromFlags default_ t fs =
    fromMaybe (default_ prms) (getFlag t fs)

-- --------------------------------------------------------------------
-- Basic options
-- --------------------------------------------------------------------

optionsForBasicStuff :: [OptDescr Flag]
optionsForBasicStuff =
  [ helpOption, verboseOption, noguiOption
  , macrosOption , lexiconOption, testSuiteOption
  , rootFeatureOption
  , outputOption
  ]

-- --------------------------------------------------------------------
-- Input files
-- --------------------------------------------------------------------

optionsForInputFiles :: [OptDescr Flag]
optionsForInputFiles =
  [ macrosOption
  , lexiconOption
  , tracesOption
  , testSuiteOption
  , fromStdinOption
  , morphInfoOption
  , instructionsOption
  , rankingOption
  , Option []    ["preselected"] (NoArg (Flag GrammarTypeFlg PreAnchored))
      "do NOT perform lexical selection - treat the grammar as the selection"
  ]

instructionsOption, macrosOption, lexiconOption, tracesOption :: OptDescr Flag

instructionsOption =
  Option [] ["instructions"] (reqArg InstructionsFileFlg id "FILE")
      "instructions file FILE"

macrosOption =
  Option ['t','m'] ["trees","macros"] (reqArg MacrosFlg id "FILE")
      "tree schemata file FILE (unanchored trees)"

lexiconOption =
  Option ['l'] ["lexicon"] (reqArg LexiconFlg id "FILE")
     "lexicon file FILE"

tracesOption =
  Option [] ["traces"] (reqArg TracesFlg id "FILE")
    "traces file FILE (list of traces to display)"

rankingOption :: OptDescr Flag
rankingOption =
  Option [] ["ranking"] (reqArg RankingConstraintsFlg id "FILE")
    "ranking constraints FILE (using Optimality Theory)"

-- --------------------------------------------------------------------
-- Output
-- --------------------------------------------------------------------

optionsForOutput :: [OptDescr Flag]
optionsForOutput =
  [ outputOption
  , Option []    ["dump"]    (noArg DumpDerivationFlg)
      "print derivation information on stdout (JSON)"
  -- same as rankingOption but with output-centric help text
  , partialOption
  , Option [] ["ranking"] (reqArg RankingConstraintsFlg id "FILE")
    "use constraints in FILE to rank output"
  ]

partialOption :: OptDescr Flag
partialOption =
 Option []    ["partial"] (noArg PartialFlg)
    "return partial result(s) if no complete solution is found"

outputOption :: OptDescr Flag
outputOption =
  Option ['o'] ["output"] (reqArg OutputFileFlg id "FILE")
    "output file FILE (stdout if unset)"

-- --------------------------------------------------------------------
-- User interface
-- --------------------------------------------------------------------

optionsForUserInterface :: [OptDescr Flag]
optionsForUserInterface =
  [ noguiOption, helpOption, versionOption
  , Option []    ["viewcmd"]  (reqArg ViewCmdFlg id "CMD")
      "XMG tree-view command"
  ]

noguiOption :: OptDescr Flag
noguiOption = Option [] ["nogui"] (noArg DisableGuiFlg)
                "disable graphical user interface"

helpOption :: OptDescr Flag
helpOption  = Option [] ["help"] (noArg HelpFlg)
                "show full list of command line switches"

versionOption :: OptDescr Flag
versionOption  = Option [] ["version"] (noArg VersionFlg)
                "display the version"

verboseOption :: OptDescr Flag
verboseOption = Option ['v'] ["verbose"] (noArg VerboseModeFlg)
                "verbose mode"

-- --------------------------------------------------------------------
-- Optimisations
-- --------------------------------------------------------------------

defaultPolarityAttrs :: String
defaultPolarityAttrs = "cat"

exampleRootFeat :: Text
exampleRootFeat = "[cat:s inv:- mode:ind|subj wh:-]"

defaultRootFeat :: Text
defaultRootFeat = "[cat:_]"

optionsForOptimisation :: [OptDescr Flag]
optionsForOptimisation =
   [ Option [] ["opts"]
         (reqArg OptimisationsFlg readOptimisations "LIST")
         "optimisations 'LIST' (--help for details)"
   , Option [] ["detect-pols"]
         (reqArg DetectPolaritiesFlg readPolarityAttrs "LIST")
         ("attributes 'LIST' (eg. \"cat idx V.tense\", default:" ++ show defaultPolarityAttrs ++ ")")
   , rootFeatureOption
   , maxResultsOption
  ]

rootFeatureOption :: OptDescr Flag
rootFeatureOption =
  Option ['r'] ["rootfeat"]
         (reqArg RootFeatureFlg (readRF . T.pack) "FEATURE")
         ("root features 'FEATURE' (eg. "
          ++ prettyStr exampleRF ++ ", default: "
          ++ prettyStr defaultRF ++ ")")
 where
   exampleRF = readRF exampleRootFeat
   defaultRF = readRF defaultRootFeat
   readRF = parseFlagWithParsec "root feature" geniFeats

coreOptimisationCodes :: [(Optimisation,String,String)]
coreOptimisationCodes =
 [ (Polarised        , "p",      "polarity filtering")
 , (NoConstraints    , "nc",     "disable semantic constraints (anti-optimisation!)")
 ]

optimisationCodes :: [(Optimisation,String,String)]
optimisationCodes =
 coreOptimisationCodes ++
 [ (PolOpts          , "pol",    equivalentTo polOpts)
 , (AdjOpts          , "adj",    equivalentTo adjOpts)
 ]
 where equivalentTo os = "equivalent to '" ++ (unwords $ map showOptCode os) ++ "'"

polOpts, adjOpts :: [Optimisation]
polOpts = [Polarised]
adjOpts = []

-- ---------------------------------------------------------------------
-- Optimisation usage info
-- ---------------------------------------------------------------------

lookupOpt:: Optimisation -> (String, String)
lookupOpt k =
 case find (\x -> k == fst3 x) optimisationCodes of
 Just (_, c, d) -> (c, d)
 Nothing -> geniBug $ "optimisation " ++  show k ++ " unknown"

showOptCode :: Optimisation -> String
showOptCode = fst.lookupOpt

describeOpt :: (Optimisation, String, String) -> String
describeOpt (_,k,d) = k ++ " - " ++ d

-- | Displays the usage text for optimisations.
--   It shows a table of optimisation codes and their meaning.
usageForOptimisations :: [String]
usageForOptimisations =
     [ "Optimisations must be passed in as a space-delimited list"
     , "(ex: --opt='p f-sem' for polarities and semantic filtering)"
     , ""
     , "Optimisations:"
     , "  " ++ unlinesTab (map describeOpt coreOptimisationCodes)
     ]
 where unlinesTab l = concat (intersperse "\n  " l)

-- ---------------------------------------------------------------------
-- Parsing optimisation stuff
-- ---------------------------------------------------------------------

-- | If we do not recognise a code, we output an error message.  We
--  also take the liberty of expanding thematic codes like 'pol'
--  into the respective list of optimisations.
readOptimisations :: String -> [Optimisation]
readOptimisations str =
  case parseOptimisations str of
    Left ick -> error $ "Unknown optimisations: " ++ (unwords ick)
    Right os -> (addif PolOpts polOpts) . (addif AdjOpts adjOpts) $ os
  where addif t x o = if (t `elem` o) then x ++ o else o

-- | Returns |Left| for any codes we don't recognise, or
--   |Right| if everything is ok.
parseOptimisations :: String -> Either [String] [Optimisation]
parseOptimisations str =
  let codes = words str
      mopts = map lookupOptimisation codes
  in if any isNothing mopts
     then Left  [ c | (c,o) <- zip codes mopts, isNothing o ]
     else Right $ map fromJust mopts

lookupOptimisation :: String -> Maybe Optimisation
lookupOptimisation code =
  liftM fst3 $ find (\x -> snd3 x == code) optimisationCodes

-- | TODO: This is a horrible and abusive use of 'error'
parseFlagWithParsec :: String -> Parser b -> Text -> b
parseFlagWithParsec description p str =
    case runParser (tillEof p) () "" str of
        Left  err -> error $ "Couldn't parse " ++ description ++ " because " ++ show err
        Right res -> res

-- --------------------------------------------------------------------
-- Builders
-- --------------------------------------------------------------------

optionsForBuilder :: [OptDescr Flag]
optionsForBuilder =
  [ Option ['b'] ["builder"]  (reqArg BuilderFlg readBuilderType "BUILDER")
      ("use as realisation engine one of: " ++ (unwords $ map show mainBuilderTypes))
  , partialOption
  , maxStepsOption
  , maxResultsOption
  ]

mainBuilderTypes :: [BuilderType]
mainBuilderTypes =
 [ SimpleBuilder, SimpleOnePhaseBuilder
 ]

-- | Hint: compose with (map toLower) to make it case-insensitive
mReadBuilderType :: String -> Maybe BuilderType
mReadBuilderType "simple"    = Just SimpleBuilder
mReadBuilderType "simple-2p" = Just SimpleBuilder
mReadBuilderType "simple-1p" = Just SimpleOnePhaseBuilder
mReadBuilderType _           = Nothing

-- | Is case-insensitive, error if unknown type
readBuilderType :: String -> BuilderType
readBuilderType b =
  case mReadBuilderType $ map toLower b of
  Just x  -> x
  Nothing -> error $ "Unknown builder type " ++ b

-- --------------------------------------------------------------------
-- Testing and profiling
-- --------------------------------------------------------------------

fromStdinOption :: OptDescr Flag
fromStdinOption =
  Option [] ["from-stdin"] (noArg FromStdinFlg) "get testcase from stdin"

testSuiteOption :: OptDescr Flag
testSuiteOption =
  Option ['s'] ["testsuite"] (reqArg TestSuiteFlg id "FILE") "test suite FILE"

maxResultsOption :: OptDescr Flag
maxResultsOption =
  Option []    ["maxresults"] (reqArg MaxResultsFlg read "INT")
      "return as soon as at least INT results are found"

maxStepsOption :: OptDescr Flag
maxStepsOption =
  Option []    ["maxsteps"] (reqArg MaxStepsFlg read "INT")
      "abort and return any results found after INT steps"

optionsForTesting :: [OptDescr Flag]
optionsForTesting =
  [ testSuiteOption
  , fromStdinOption
  , Option []    ["testcase"]
      (reqArg TestCaseFlg T.pack "STRING")
      "run test case STRING"
  , Option []    ["timeout"] (reqArg TimeoutFlg read "SECONDS")
      "time out after SECONDS seconds"
  , maxResultsOption
  , maxStepsOption
  , Option []    ["metrics"] (optArg MetricsFlg ["default"] words "LIST")
      "keep track of performance metrics: (default: iterations comparisons chart_size)"
  , Option []    ["statsfile"] (reqArg StatsFileFlg id "FILE")
      "write performance data to file FILE (stdout if unset)"
  , Option []    ["batchdir"]    (reqArg BatchDirFlg id "DIR")
      "batch process the test suite and save results to DIR"
  , Option []    ["earlydeath"]    (noArg EarlyDeathFlg)
      "exit on first case with no results (batch processing) "
 ]

-- --------------------------------------------------------------------
-- Morphology
-- --------------------------------------------------------------------

optionsForMorphology :: [OptDescr Flag]
optionsForMorphology =
  [ morphInfoOption
  , Option []    ["morphcmd"]  (reqArg MorphCmdFlg id "CMD")
      "morphological post-processor CMD (default: unset)"
  ]

morphInfoOption :: OptDescr Flag
morphInfoOption = Option [] ["morphinfo"] (reqArg MorphInfoFlg id "FILE")
  "morphological features FILE (default: unset)"

-- ====================================================================
-- Scripting GenI
-- ====================================================================

-- | Update the internal instructions list, test suite and case
--   according to the contents of an instructions file.
--
--   Basic approach
--
--   * we always have instructions: if no instructions file, is specified
--     we infer virtual instructions from the test suite flag
--   * the testsuite and testcase flags are focusing tools, they pick out
--     a subset from the instructions
processInstructions :: Params -> IO Params
processInstructions config = do
    instructions <- case getFlagP InstructionsFileFlg config of
                      Nothing -> return fakeInstructions
                      Just f  -> instructionsFile `fmap` T.readFile f
    let updateInstructions = setFlagP TestInstructionsFlg instructions
        -- we have to set a test suite in case the user only supplies
        -- an instructions argument so that NLP.GenI.loadEverything
        -- knows that the user has given us a suite to load
        updateTestSuite p =
          if hasFlagP TestSuiteFlg p then p
             else case (fst `fmap` listToMaybe instructions) of
                   Just s  -> setFlagP TestSuiteFlg s p
                   Nothing -> p
    return . updateTestSuite . updateInstructions $ config
  where
    fakeInstructions :: [Instruction]
    fakeInstructions =
         let cases = singleton <$> getFlagP TestCaseFlg config
             mkInstr xs = singleton (xs, cases)
         in maybe [] mkInstr $ getFlagP TestSuiteFlg config

instructionsFile :: Text -> [Instruction]
instructionsFile =
    mapMaybe inst . T.lines
  where
    inst l = case T.words (T.takeWhile (/= '%') l) of
                 []     -> Nothing
                 [f]    -> Just (T.unpack f, Nothing)
                 (f:cs) -> Just (T.unpack f, Just cs)

-- ====================================================================
-- Configuration file
-- ====================================================================

readGlobalConfig :: IO (Maybe YamlLight)
readGlobalConfig = do
  geniCfgDir <- getAppUserDataDirectory "geni"
  let globalCfg = geniCfgDir </> "config.yaml"
  hasCfg <- doesFileExist globalCfg
  if hasCfg then Just `fmap` parseYamlFile globalCfg 
            else return Nothing

data LoggerConfig = LoggerConfig { lcName      :: String
                                 , lcPriority  :: Priority
                                 , lcHandler   :: LogTo
                                 , lcFormatter :: LogFmt
                                 }
 deriving Show

data LogTo = LogToFile FilePath | LogToErr
 deriving Show

data LogFmt = LogFmtNull | LogFmtSimple String
 deriving Show

logDefaultConfig :: String -> LoggerConfig
logDefaultConfig n = LoggerConfig
    { lcName      = n
    , lcPriority  = DEBUG
    , lcHandler   = LogToErr
    , lcFormatter = LogFmtNull
    }

setLoggers :: YamlLight -> IO ()
setLoggers y = do
    -- it seems we need to explicitly create the root logger
    -- we set this to the lowest priority because we want the user to
    -- be able to set the priority on their loggers as low as they want 
    updateGlobalLogger "" $ setLevel DEBUG
                          . setHandlers noHandlers
    mapM_ setGeniHandler $ fromMaybe [globalDefault] (loggerConfig y)
  where
    noHandlers :: [GenericHandler ()]
    noHandlers = []
    globalDefault = (logDefaultConfig "NLP.GenI") { lcPriority = INFO }

setGeniHandler :: LoggerConfig -> IO ()
setGeniHandler lc = do
    h <- flip setFormatter fmttr <$> handler (lcPriority lc)
    updateGlobalLogger (lcName lc) (setHandlers [h])
  where
    handler = case lcHandler lc of
                LogToFile f -> fileHandler f
                LogToErr    -> streamHandler stderr
    --
    fmttr = case lcFormatter lc of
              LogFmtSimple str -> simpleLogFormatter str
              LogFmtNull       -> nullFormatter

instance Read LogTo where
  readsPrec _ (dropPrefix "stderr"  -> ("", x)) = [ (LogToErr, x) ]
  readsPrec p (dropPrefix "file"    -> ("", x)) = map (first LogToFile) (readsQuotedStringPrec p x)
  readsPrec _ _ = []

instance Read LogFmt where
  readsPrec _ (dropPrefix "null"     -> ("", x)) = [ (LogFmtNull, x) ]
  readsPrec p (dropPrefix "simple"   -> ("", x)) = map (first LogFmtSimple) (readsQuotedStringPrec p x)
  readsPrec _ _ = []

readsQuotedStringPrec :: Int -> String -> [ (String, String) ]
readsQuotedStringPrec p x@(h:_) | isSpace h =
    case dropWhile isSpace x of
      xs@('"':_)                                -> readsPrec p xs
      (break isSpace -> y) | not (null (fst y)) -> [y]
      _                                         -> []
readsQuotedStringPrec _ _                   = []

loggerConfig :: YamlLight -> Maybe [LoggerConfig]
loggerConfig yaml = lookupYL "logging" yaml
                  >>= unSeq
                  >>= mapM unMap
                  >>= mapM readOne
 where
   readOne :: Map.Map YamlLight YamlLight -> Maybe LoggerConfig 
   readOne m = do
     let name = fromMaybe "NLP.GenI" (get Just "name" m)
     return $ updater "level"   m (\x l -> l { lcPriority = x })
            . updater "handler" m (\x l -> l { lcHandler  = x })
            . updater "format"  m (\x l -> l { lcFormatter = x })
            $ logDefaultConfig name
   updater str m fn = maybe id fn (get maybeRead str m)
   get f x m = Map.lookup x m >>= unStr >>= (f . BC.unpack)

instance IsString YamlLight where
  fromString = YStr . fromString

-- ----------------------------------------------------------------------
--
-- ----------------------------------------------------------------------

singleton :: a -> [a]
singleton = pure

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
  [(x, rest)] | all isSpace rest -> Just x
  _         -> Nothing

dropPrefix :: Eq a => [a] -> [a] -> ([a],[a])
dropPrefix (x:xs) (y:ys) | x == y    = dropPrefix xs ys
dropPrefix left right = (left,right)
