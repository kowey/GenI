{-# LANGUAGE ExistentialQuantification #-}
-- GenI surface realiser
-- Copyright (C) 2009 Eric Kow
--
-- This module can be treated as public domain

module NLP.GenI.Flags where

-- This module exists purely for the purpose of making it more convenient
-- to modify the flag set in GenI (by eliminating the need to explicitly
-- export the types for individual flags).  To add a flag:
--
--  1. add a type here
--  2. write the help text in NLP.GenI.Configuration
--  3. add the getopt stuff and find a section for the flag

import Data.List ( find )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe ( catMaybes, fromJust )
import Data.Typeable

import NLP.GenI.General ( Interval )
import NLP.GenI.FeatureStructures ( Flist )
import NLP.GenI.GeniVal ( GeniVal )
import NLP.GenI.PolarityTypes

-- ----------------------------------------------------------------------
-- configuration stuff
-- this stuff really belongs in NLP.GenI.Configuration but we move it here
-- to avoid an import cycle (sigh)
-- ----------------------------------------------------------------------

data Optimisation = PolOpts
                  | AdjOpts
                  | Polarised
                  | NoConstraints
                  | SemFiltered
                  | EarlyNa
  deriving (Show,Eq,Typeable)

type Instruction = (FilePath, Maybe [String])

data BuilderType = SimpleBuilder | SimpleOnePhaseBuilder
     deriving (Eq, Typeable)

data GrammarType = GeniHand    -- ^ geni's text format
                 | PreCompiled -- ^ built into geni, no parsing needed
                 | PreAnchored -- ^ lexical selection already done
     deriving (Show, Eq, Typeable)

instance Show BuilderType where
  show SimpleBuilder         = "simple-2p"
  show SimpleOnePhaseBuilder = "simple-1p"

-- ----------------------------------------------------------------------
-- flag core
-- ----------------------------------------------------------------------

{- |
Flags are GenI's internal representation of command line arguments.  We
use phantom existential types (?) for representing GenI flags.  This
makes it simpler to do things such as ``get the value of the MacrosFlg''
whilst preserving type safety (we always know that MacrosFlg is
associated with String).  The alternative would be writing getters and
setters for each flag, and that gets really boring after a while.
-}
data Flag = forall f x . (Eq f, Show f, Show x, Typeable f, Typeable x) =>
     Flag (x -> f) x deriving (Typeable)

instance Show Flag where
 show (Flag f x) = "Flag " ++ show (f x)

instance Eq Flag where
 (Flag f1 x1) == (Flag f2 x2)
   | (typeOf f1 == typeOf f2) && (typeOf x1 == typeOf x2) =
       (fromJust . cast . f1 $ x1) == (f2 x2)
   | otherwise = False

isFlag :: (Typeable f, Typeable x) => (x -> f) -> Flag -> Bool
isFlag f1 (Flag f2 _) = typeOf f1 == typeOf f2

hasFlag :: (Typeable f, Typeable x) => (x -> f) -> [Flag] -> Bool
hasFlag = any . isFlag

deleteFlag :: (Typeable f, Typeable x) => (x -> f) -> [Flag] -> [Flag]
deleteFlag f = filter (not.(isFlag f))

setFlag :: (Eq f, Show f, Show x, Typeable f, Typeable x) => (x -> f) -> x -> [Flag] -> [Flag]
setFlag f v fs = (Flag f v) : tl where tl = deleteFlag f fs

getFlag :: (Show f, Show x, Typeable f, Typeable x)  => (x -> f) -> [Flag] -> Maybe x
getFlag f fs = do (Flag _ v) <- find (isFlag f) fs ; cast v

getAllFlags :: (Show f, Show x, Typeable f, Typeable x)  => (x -> f) -> [Flag] -> [x]
getAllFlags f fs = catMaybes [ cast v | flg@(Flag _ v) <- fs, isFlag f flg ]

-- ----------------------------------------------------------------------
-- Below are just the individual flags, which unfortunately have to be
-- defined as separate data types because of our fancy existential
-- data type code.
-- ----------------------------------------------------------------------

data BatchDirFlg = BatchDirFlg FilePath deriving (Eq, Show, Typeable)
data DisableGuiFlg = DisableGuiFlg () deriving (Eq, Show, Typeable)
data DetectPolaritiesFlg = DetectPolaritiesFlg (Set.Set PolarityAttr) deriving (Eq, Show, Typeable)
data DumpDerivationFlg = DumpDerivationFlg () deriving (Eq, Show, Typeable)
data EarlyDeathFlg = EarlyDeathFlg () deriving (Eq, Show, Typeable)
data ExtraPolaritiesFlg = ExtraPolaritiesFlg (Map.Map PolarityKey Interval) deriving (Eq, Show, Typeable)
data FromStdinFlg = FromStdinFlg () deriving (Eq, Show, Typeable)
data HelpFlg = HelpFlg () deriving (Eq, Show, Typeable)
data InstructionsFileFlg = InstructionsFileFlg FilePath deriving (Eq, Show, Typeable)
data LexiconFlg = LexiconFlg FilePath deriving (Eq, Show, Typeable)
data MacrosFlg = MacrosFlg FilePath deriving (Eq, Show, Typeable)
data TracesFlg = TracesFlg FilePath deriving (Eq, Show, Typeable)
data MetricsFlg = MetricsFlg [String] deriving (Eq, Show, Typeable)
data MorphCmdFlg = MorphCmdFlg String deriving (Eq, Show, Typeable)
data MorphInfoFlg = MorphInfoFlg FilePath deriving (Eq, Show, Typeable)
data OptimisationsFlg = OptimisationsFlg [Optimisation] deriving (Eq, Show, Typeable)
data OutputFileFlg = OutputFileFlg String deriving (Eq, Show, Typeable)
data PartialFlg = PartialFlg () deriving (Eq, Show, Typeable)
data RankingConstraintsFlg = RankingConstraintsFlg FilePath deriving (Eq, Show, Typeable)
data RegressionTestModeFlg = RegressionTestModeFlg () deriving (Eq, Show, Typeable)
data RootFeatureFlg = RootFeatureFlg (Flist GeniVal) deriving (Eq, Show, Typeable)
data RunUnitTestFlg = RunUnitTestFlg () deriving (Eq, Show, Typeable)
data NoLoadTestSuiteFlg = NoLoadTestSuiteFlg () deriving (Eq, Show, Typeable)
data StatsFileFlg = StatsFileFlg FilePath deriving (Eq, Show, Typeable)
data TestCaseFlg = TestCaseFlg String deriving (Eq, Show, Typeable)
data TestInstructionsFlg = TestInstructionsFlg [Instruction] deriving (Eq, Show, Typeable)
data TestSuiteFlg = TestSuiteFlg FilePath deriving (Eq, Show, Typeable)
data TimeoutFlg = TimeoutFlg Integer deriving (Eq, Show, Typeable)
data VerboseModeFlg = VerboseModeFlg () deriving (Eq, Show, Typeable)
data VersionFlg = VersionFlg () deriving (Eq, Show, Typeable)
data ViewCmdFlg = ViewCmdFlg String deriving (Eq, Show, Typeable)
data BuilderFlg = BuilderFlg  BuilderType deriving (Eq, Show, Typeable)
data GrammarTypeFlg = GrammarTypeFlg GrammarType deriving (Eq, Show, Typeable)
-- the WeirdFlg exists strictly to please OS X when you launch
-- GenI in an application bundle (double-click)... for some
-- reason it wants to pass an argument to -p
data WeirdFlg = WeirdFlg String deriving (Eq, Show, Typeable)
