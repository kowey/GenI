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

\chapter{Command line arguments}

\begin{code}
module NLP.GenI.Configuration
  ( Params(..), GrammarType(..), BuilderType(..), Instruction, Flag
  -- flags
  , BatchDirFlg(..)
  , DisableGuiFlg(..)
  , EarlyDeathFlg(..)
  , ExtraPolaritiesFlg(..)
  , FromStdinFlg(..)
  , HelpFlg(..)
  , IgnoreSemanticsFlg(..)
  , InstructionsFileFlg(..)
  , LexiconFlg(..)
  , MacrosFlg(..)
  , MaxTreesFlg(..)
  , MetricsFlg(..)
  , MorphCmdFlg(..)
  , MorphInfoFlg(..)
  , MorphLexiconFlg(..)
  , NoLoadTestSuiteFlg(..)
  , OptimisationsFlg(..)
  , OutputFileFlg(..)
  , PartialFlg(..)
  , RegressionTestModeFlg(..)
  , RootFeatureFlg(..)
  , StatsFileFlg(..)
  , TestCaseFlg(..)
  , TestInstructionsFlg(..)
  , TestSuiteFlg(..)
  , TimeoutFlg(..)
  , TracesFlg(..)
  , VerboseModeFlg(..)
  , ViewCmdFlg(..)
  --
  , mainBuilderTypes
  , getFlagP, getListFlagP, setFlagP, hasFlagP, deleteFlagP, hasOpt, polarised
  , getFlag, setFlag, hasFlag
  , Optimisation(..)
  , rootcatfiltered, semfiltered
  , isIaf
  , emptyParams, defineParams
  , treatArgs, treatStandardArgs, treatArgsWithParams, treatStandardArgsWithParams
  , processInstructions
  , optionsForStandardGenI
  , optionsForBasicStuff, optionsForOptimisation, optionsForMorphology, optionsForInputFiles
  , optionsForBuilder, optionsForTesting
  , nubBySwitches
  , noArg, reqArg, optArg
  , parseFlagWithParsec
  -- re-exports
  , module System.Console.GetOpt
  , Typeable
  )
where
\end{code}

\ignore{
\begin{code}
import qualified Data.Map as Map

import Control.Monad ( liftM )
import Data.Char ( toLower )
import Data.Maybe ( listToMaybe, mapMaybe )
import Data.Typeable ( Typeable, typeOf, cast )
import System.Console.GetOpt
import System.Exit ( exitFailure, exitWith, ExitCode(..) )
import Data.List  ( find, intersperse, nubBy )
import Data.Maybe ( catMaybes, fromMaybe, isNothing, fromJust )
import Text.ParserCombinators.Parsec ( runParser, CharParser )

import NLP.GenI.Btypes ( GeniVal(GConst), Flist, showFlist, )
import NLP.GenI.General ( geniBug, fst3, snd3, Interval )
import NLP.GenI.GeniParsers ( geniFeats, geniPolarities )
\end{code}
}

% --------------------------------------------------------------------
% Code for debugging. (should be latex-commented
% when not in use)
% --------------------------------------------------------------------

%\begin{code}
%import Debug.Trace
%\end{code}

% --------------------------------------------------------------------
% Params
% --------------------------------------------------------------------

\begin{code}
-- | Holds the specification for how Geni should be run, its input
--   files, etc.  This is the stuff that would normally be found in
--   the configuration file.
data Params = Prms{
  grammarType    :: GrammarType,
  builderType    :: BuilderType,
  geniFlags      :: [Flag]
} deriving (Show)

hasOpt :: Optimisation -> Params -> Bool
hasOpt o p = maybe False (elem o) $ getFlagP OptimisationsFlg p

polarised, isIaf :: Params -> Bool
rootcatfiltered, semfiltered :: Params -> Bool
polarised    = hasOpt Polarised
isIaf        = hasOpt Iaf
semfiltered  = hasOpt SemFiltered
rootcatfiltered = hasOpt RootCatFiltered

hasFlagP    :: (Typeable f, Typeable x) => (x -> f) -> Params -> Bool
deleteFlagP :: (Typeable f, Typeable x) => (x -> f) -> Params -> Params
setFlagP    :: (Eq f, Show f, Show x, Typeable f, Typeable x) => (x -> f) -> x -> Params -> Params
getFlagP    :: (Show f, Show x, Typeable f, Typeable x) => (x -> f) -> Params -> Maybe x
getListFlagP :: (Show f, Show x, Typeable f, Typeable x) => ([x] -> f) -> Params -> [x]

hasFlagP f      = hasFlag f . geniFlags
deleteFlagP f p = p { geniFlags = deleteFlag f (geniFlags p) }
setFlagP f v p  = p { geniFlags = setFlag f v (geniFlags p) }
getFlagP f     = getFlag f . geniFlags
getListFlagP f = fromMaybe [] . getFlagP f
-- | The default parameters configuration
emptyParams :: Params
emptyParams = Prms {
  builderType   = SimpleBuilder,
  grammarType   = GeniHand,
  geniFlags     = [ Flag ViewCmdFlg "ViewTAG"
                  , Flag RootFeatureFlg defaultRootFeat ]
}
\end{code}

% --------------------------------------------------------------------
\section{Command line arguments}
% --------------------------------------------------------------------

Command line arguments can be specified in the GNU style, for example
\texttt{--foo=bar} or \texttt{--foo bar}, or \texttt{-f bar} when a
short switch is available.  For more information, type \texttt{geni
--help}.

\begin{code}
-- | Uses the GetOpt library to process the command line arguments.
-- Note that we divide them into basic and advanced usage.
optionsForStandardGenI, optionsAdvanced :: [OptDescr Flag]
optionsForStandardGenI = nubBySwitches $ optionsForBasicStuff ++ optionsAdvanced
          ++ -- FIXME: weird mac stuff
          [ Option ['p']    []  (reqArg WeirdFlg id "CMD") "" ]

optionsAdvanced = nubBySwitches $
        optionsForUserInterface
     ++ optionsForInputFiles
     ++ optionsForOutput
     ++ optionsForOptimisation
     ++ optionsForBuilder
     ++ optionsForTesting
     ++ optionsForMorphology
     ++ optionsForIgnoreSem

getSwitches :: OptDescr a -> ([Char],[String])
getSwitches (Option s l _ _) = (s,l)

nubBySwitches :: [OptDescr a] -> [OptDescr a]
nubBySwitches = nubBy (\x y -> getSwitches x == getSwitches y)
\end{code}

\subsection{Essential arguments}

See also section \ref{sec:optimisations} for more details on
optimisations.

% FIXME: what would be great is some special processing of the
% code below so that the documentation writes itself

\begin{code}
-- GetOpt wrappers
noArg :: forall f . (Eq f, Show f, Typeable f)
      => (() -> f) -> ArgDescr Flag
noArg  s = NoArg (Flag s ())

reqArg :: forall f x . (Eq f, Show f, Typeable f, Eq x, Show x, Typeable x)
       => (x -> f)      -- ^ flag
       -> (String -> x) -- ^ string reader for flag (probably |id| if already a String)
       -> String        -- ^ description
       -> ArgDescr Flag
reqArg s fn desc = ReqArg (\x -> Flag s (fn x)) desc

optArg :: forall f x . (Eq f, Show f, Typeable f, Eq x, Show x, Typeable x)
       => (x -> f)       -- ^ flag
       -> x              -- ^ default value
       -> (String -> x)  -- ^ string reader (as in @reqArg@)
       -> String         -- ^ description
       -> ArgDescr Flag
optArg s def fn desc = OptArg (\x -> Flag s (maybe def fn x)) desc
\end{code}

\begin{code}
-- -------------------------------------------------------------------
-- Parsing command line arguments
-- -------------------------------------------------------------------

usage :: Bool -- ^ advanced
      -> String
usage adv =
 let header   = "Usage: geni [OPTION...]"
     body     = basic ++ if adv then ("\n\n" ++ advanced) else ""
     example  = "Example:\n" ++
       " geni -m examples/ej/mac -l examples/ej/lexicon -s examples/ej/suite\n"
     basic    = usageInfo header optionsForBasicStuff
     advanced = usageInfo "Advanced options (note: all LIST are space delimited)" optionsAdvanced
                ++ usageForOptimisations
 in header ++ body ++ "\n\n" ++ example

treatStandardArgs :: [String] -> IO Params
treatStandardArgs argv = treatStandardArgsWithParams argv emptyParams

treatStandardArgsWithParams :: [String] -> Params -> IO Params
treatStandardArgsWithParams = treatArgsWithParams optionsForStandardGenI

treatArgs :: [OptDescr Flag] -> [String] -> IO Params
treatArgs options argv = treatArgsWithParams options argv emptyParams

treatArgsWithParams :: [OptDescr Flag] -> [String] -> Params -> IO Params
treatArgsWithParams options argv initParams =
   case getOpt Permute options argv of
     (os,_,[]  )
       | hasFlag HelpFlg os ->
           do putStrLn $ usage True
              exitWith ExitSuccess
       | hasFlag DisableGuiFlg os
         && notHasFlag TestCaseFlg os
         && notHasFlag RegressionTestModeFlg os
         && notHasFlag BatchDirFlg os
         && notHasFlag FromStdinFlg os ->
           do putStrLn $ "GenI must either be run in graphical mode, "
                         ++ "in regression mode, with a test case specified, with --from-stdin,"
                         ++ "or with a batch directory specified"
              exitFailure
       | otherwise ->
           return $ defineParams os initParams
     (_,_,errs) -> ioError (userError $ concat errs ++ usage False)
  where notHasFlag f l = not $ hasFlag f l

defineParams :: [Flag] -> Params -> Params
defineParams flgs prms =
  (\p -> foldr setDefault p $ geniFlags prms)
  . maybeSetMaxTrees
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
  maybeSetMaxTrees p =
    if hasFlagP IgnoreSemanticsFlg p && (not $ hasFlagP MaxTreesFlg p)
    then setFlagP MaxTreesFlg 5 p else p
\end{code}

\section{Options by theme}
\label{sec:fancy_parameters}

Note that you might see an option described in more than one place
because it falls into multiple categories.

% --------------------------------------------------------------------
\subsection{Basic options}
% --------------------------------------------------------------------

\begin{code}
optionsForBasicStuff :: [OptDescr Flag]
optionsForBasicStuff =
  [ helpOption, verboseOption, noguiOption
  , macrosOption , lexiconOption, testSuiteOption
  , outputOption
  ]
\end{code}

% --------------------------------------------------------------------
\subsection{Input files}
% --------------------------------------------------------------------

\begin{code}
optionsForInputFiles :: [OptDescr Flag]
optionsForInputFiles =
  [ macrosOption
  , lexiconOption
  , tracesOption
  , testSuiteOption
  , fromStdinOption
  , morphInfoOption
  , instructionsOption
  , outputOption
  , Option []    ["preselected"] (NoArg (Flag GrammarTypeFlg PreAnchored))
      "do NOT perform lexical selection - treat the grammar as the selection"
  ]

instructionsOption, macrosOption, lexiconOption, tracesOption, outputOption :: OptDescr Flag

instructionsOption =
  Option [] ["instructions"] (reqArg InstructionsFileFlg id "FILE")
      "instructions file FILE"

macrosOption =
  Option ['m'] ["macros"] (reqArg MacrosFlg id "FILE")
      "macros file FILE (unanchored trees)"

lexiconOption =
  Option ['l'] ["lexicon"] (reqArg LexiconFlg id "FILE")
     "lexicon file FILE"

tracesOption =
  Option [] ["traces"] (reqArg TracesFlg id "FILE")
    "traces file FILE (list of traces to display)"

outputOption =
  Option ['o'] ["output"] (reqArg OutputFileFlg id "FILE")
    "output file FILE (stdout if unset)"
\end{code}

% --------------------------------------------------------------------
\subsection{Output}
% --------------------------------------------------------------------

\begin{code}
optionsForOutput :: [OptDescr Flag]
optionsForOutput =
  [ outputOption
  , Option []    ["partial"] (noArg PartialFlg)
      "return partial result(s) if no complete solution is found"
  ]
\end{code}

% --------------------------------------------------------------------
\subsection{User interface}
% --------------------------------------------------------------------

\begin{code}
optionsForUserInterface :: [OptDescr Flag]
optionsForUserInterface =
  [ noguiOption, helpOption
  , Option []    ["regression"] (noArg RegressionTestModeFlg)
      "Run in regression testing mode (needs grammar, etc)"
  , Option []    ["viewcmd"]  (reqArg ViewCmdFlg id "CMD")
      "XMG tree-view command"
  ]

verboseOption, noguiOption, helpOption :: OptDescr Flag
noguiOption = Option [] ["nogui"] (noArg DisableGuiFlg)
                "disable graphical user interface"
helpOption  = Option [] ["help"] (noArg HelpFlg)
                "show full list of command line switches"
verboseOption = Option ['v'] ["verbose"] (noArg VerboseModeFlg)
                "verbose mode"
\end{code}

% --------------------------------------------------------------------
\subsection{Optimisations}
% --------------------------------------------------------------------

\begin{description}
\item[opt]
  The opt switch lets you specify a list of optimisations
  that GenI should use, for example, \texttt{--opt='pol S i'}.
  We associate each optimisation with a short code like 'i' for
  ``index accessibility filtering''.  This code is what the
  user passes in, and is sometimes used by GenI to tell the
  user which optimisations it's using.  See \texttt{geni
    --help} for more detail on the codes.

  Optimisations can be accumulated.  For example, if you say something
  like \texttt{--opt='foo bar' --opt='quux'} it is the same as saying
  \texttt{--opt='foo bar quux'}.

  Note that we also have two special thematic codes ``pol'' and
  ``adj'' which tell GenI that it should enable all the
  polarity-related, and all the adjunction-related
  optimisations respectively.

\item[rootfeat]
  No results?  Make sure your rootfeat are set correctly.  GenI
  will reject all sentences whose root category does not unify
  with the rootfeat, the default of which is:
\begin{includecodeinmanual}
\begin{code}
defaultRootFeat :: Flist
defaultRootFeat =
  [ ("cat" , GConst ["s"])
  , ("inv" , GConst ["-"])
  , ("mode", GConst ["ind","subj"])
  , ("wh"  , GConst ["-"])
  ]
\end{code}
\end{includecodeinmanual}

  You can set rootfeat to be empty (\verb![]!) if you want, in
  which case the realiser proper will return all results; but
  note that if you want to use polarity filtering, you must at
  least specify a value for the \verb!cat! feature.

\item[extrapols]
  Allows to to preset some polarities.  There's not very much use for
  this, in my opinion.  Most likely, what you really want is rootfeat.
\end{description}

\begin{code}
optionsForOptimisation :: [OptDescr Flag]
optionsForOptimisation =
   [ Option [] ["opts"]
         (reqArg OptimisationsFlg readOptimisations "LIST")
         "optimisations 'LIST' (--help for details)"
   , Option [] ["rootfeat"]
         (reqArg RootFeatureFlg readRF "FEATURE")
         ("root features 'FEATURE' (for polarities, default:"
          ++ showFlist defaultRF ++ ")")
  , Option [] ["extrapols"]
         (reqArg ExtraPolaritiesFlg readPolarities "STRING")
         "preset polarities (normally, you should use rootfeat instead)"
  ]
  where
   defaultRF = getListFlagP RootFeatureFlg emptyParams
   readRF = parseFlagWithParsec "root feature" geniFeats
   readPolarities = parseFlagWithParsec "polarity string" geniPolarities

data Optimisation =
  PolOpts | AdjOpts | Polarised | NoConstraints |
  RootCatFiltered | SemFiltered | Iaf {- one phase only! -}
  deriving (Show,Eq,Typeable)

coreOptimisationCodes :: [(Optimisation,String,String)]
coreOptimisationCodes =
 [ (Polarised        , "p",      "polarity filtering")
 , (SemFiltered      , "f-sem",  "semantic filtering (two-phase only)")
 , (RootCatFiltered  , "f-root", "filtering on root node (two-phase only)")
 , (Iaf              , "i",      "index accesibility filtering (one-phase only)")
 , (NoConstraints    , "nc",     "disable semantic constraints (anti-optimisation!)")
 ]

optimisationCodes :: [(Optimisation,String,String)]
optimisationCodes =
 coreOptimisationCodes ++
 [ (SemFiltered      , "S",      "semantic filtering (same as f-sem)")
 , (PolOpts          , "pol",    equivalentTo polOpts)
 , (AdjOpts          , "adj",    equivalentTo adjOpts)
 ]
 where equivalentTo os = "equivalent to '" ++ (unwords $ map showOptCode os) ++ "'"

polOpts, adjOpts :: [Optimisation]
polOpts = [Polarised]
adjOpts = [RootCatFiltered, SemFiltered]
\end{code}

\begin{code}
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
usageForOptimisations :: String
usageForOptimisations = "\n"
     ++ "List of optimisations.\n"
     ++ "(ex: --opt='p f-sem' for polarities and semantic filtering)\n"
     ++ "\n"
     ++ "Optimisations:\n"
     ++ "  " ++ unlinesTab (map describeOpt coreOptimisationCodes) ++ "\n"
 where unlinesTab l = concat (intersperse "\n  " l)
\end{code}

\begin{code}
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

parseFlagWithParsec :: String -> CharParser () b -> String -> b
parseFlagWithParsec description p str =
 case runParser p () "" str of
 Left  err -> error $ "Couldn't parse " ++ description ++ " because " ++ show err
 Right res -> res
\end{code}

% --------------------------------------------------------------------
\subsection{Builders}
% --------------------------------------------------------------------

\begin{description}
\item[builder]
  A builder is basically a surface realisation algorithm.  Some
  builders do not differ by very much.  For example, the Earley and CKY builders
  are more or less the same from GenI's point of view, except with one little
  parameter to tweak.
\end{description}

\begin{code}
data BuilderType = NullBuilder |
                   SimpleBuilder | SimpleOnePhaseBuilder |
                   CkyBuilder | EarleyBuilder
     deriving (Eq, Typeable)

instance Show BuilderType where
  show NullBuilder           = "null"
  show SimpleBuilder         = "simple-2p"
  show SimpleOnePhaseBuilder = "simple-1p"
  show CkyBuilder            = "CKY"
  show EarleyBuilder         = "Earley"

optionsForBuilder :: [OptDescr Flag]
optionsForBuilder =
  [ Option ['b'] ["builder"]  (reqArg BuilderFlg readBuilderType "BUILDER")
      ("use as realisation engine one of: " ++ (unwords $ map show mainBuilderTypes))
  ]

mainBuilderTypes :: [BuilderType]
mainBuilderTypes =
 [ SimpleBuilder, SimpleOnePhaseBuilder
 , CkyBuilder, EarleyBuilder]

-- | Hint: compose with (map toLower) to make it case-insensitive
mReadBuilderType :: String -> Maybe BuilderType
mReadBuilderType "null"      = Just NullBuilder
mReadBuilderType "cky"       = Just CkyBuilder
mReadBuilderType "earley"    = Just EarleyBuilder
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

\end{code}

% --------------------------------------------------------------------
\subsection{Testing and profiling}
% --------------------------------------------------------------------

\begin{code}
fromStdinOption :: OptDescr Flag
fromStdinOption =
  Option [] ["from-stdin"] (noArg FromStdinFlg) "get testcase from stdin"

testSuiteOption :: OptDescr Flag
testSuiteOption =
  Option ['s'] ["testsuite"] (reqArg TestSuiteFlg id "FILE") "test suite FILE"

optionsForTesting :: [OptDescr Flag]
optionsForTesting =
  [ testSuiteOption
  , fromStdinOption
  , Option []    ["testcase"]   (reqArg TestCaseFlg id "STRING")
      "run test case STRING"
  , Option []    ["timeout"] (reqArg TimeoutFlg read "SECONDS")
      "time out after SECONDS seconds"
  , Option []    ["metrics"] (optArg MetricsFlg ["default"] words "LIST")
      "keep track of performance metrics: (default: iterations comparisons chart_size)"
  , Option []    ["statsfile"] (reqArg StatsFileFlg id "FILE")
      "write performance data to file FILE (stdout if unset)"
  , Option []    ["batchdir"]    (reqArg BatchDirFlg id "DIR")
      "batch process the test suite and save results to DIR"
  , Option []    ["earlydeath"]    (noArg EarlyDeathFlg)
      "exit on first case with no results (batch processing) "
 ]
\end{code}

% --------------------------------------------------------------------
\subsection{Morphology}
% --------------------------------------------------------------------

GenI provides two options for morphology: either you use an external
inflection program (morphcmd), or you pass in a morphological lexicon
(morphlexicon) and in doing so, use GenI's built in inflecter.  The
GenI internal morphology mechanism is a simple and stupid lookup-and-
unify table, so you probably don't want to use it if you have a huge
lexicon.

\begin{description}
\item[morphcmd] specifies the program used for morphology.  Literate
GenI \cite{literateGeni} has a chapter describing how that program must work.
It will mostly likely be a script you wrote to wrap around some off-the-shelf
software.
\item[morphlexicon] specifies a morphological lexicon for use by
GenI's internal morphological generator.  Specifying this option will
cause the morphcmd flag to be ignored.
\item[morphinfo] tells GenI which literals in the input semantics are
to be used by the morphological \emph{pre-}processor.  The pre-processor
strips these features from the input and fiddles with the elementary
trees used by GenI so that the right features get attached to the leaf
nodes.  An example of a ``morphological'' literal is something like
\texttt{past(p)}.
\end{description}

\begin{code}
optionsForMorphology :: [OptDescr Flag]
optionsForMorphology =
  [ morphInfoOption
  , Option []    ["morphcmd"]  (reqArg MorphCmdFlg id "CMD")
      "morphological post-processor CMD (default: unset)"
  , Option []    ["morphlexicon"]  (reqArg MorphLexiconFlg id "FILE")
      "morphological lexicon FILE (default: unset) - overrides morphcmd!"
  ]

morphInfoOption :: OptDescr Flag
morphInfoOption = Option [] ["morphinfo"] (reqArg MorphInfoFlg id "FILE")
  "morphological lexicon FILE (default: unset)"
\end{code}

% --------------------------------------------------------------------
\subsection{Ignore semantics mode}
% --------------------------------------------------------------------

\begin{description}
\item[ignoresem] is a special generation mode for systematically
churning out any sentences that the grammar can produce, without
using an input semantics.  \textbf{Note}: This was implemented by Jackie
Lai (see patches around 2005-06-16), but has been horribly broken by
Eric sometime before 2006-08.  Please let us know if you actually use
this thing, so that we can fix it.
\item[maxtrees] limits ignoresem mode by restricting the size of its
derivation trees (in number of elementary trees).  Otherwise, GenI
would just spin around exploring an infinite number of sentences.
If you don't specify a maxtrees under ignoresem mode, we'll use a
default of 5.  Note that maxtrees also works in normal generation
mode.  It could be a useful way of saying ``give me only really
small sentences''.
\end{description}

\begin{code}
optionsForIgnoreSem :: [OptDescr Flag]
optionsForIgnoreSem =
  [ Option []    ["ignoresem"]   (noArg IgnoreSemanticsFlg)
      "ignore all semantic information"
  , Option []    ["maxtrees"]   (reqArg MaxTreesFlg read "INT")
      "max tree size INT by number of elementary trees"
  ]
\end{code}

% --------------------------------------------------------------------
\subsection{Other options}
% --------------------------------------------------------------------

\begin{code}
data GrammarType = GeniHand    -- ^ geni's text format
                 | PreCompiled -- ^ built into geni, no parsing needed
                 | PreAnchored -- ^ lexical selection already done
     deriving (Show, Eq, Typeable)
\end{code}

% ====================================================================
\section{Scripting GenI}
% ====================================================================

Any input that you give to GenI will be interpreted as a list of test
suites (and test cases that you want to run).  Each line has the format
\texttt{path/to/test-suite case1 case2 .. caseN}.   You can omit the
test cases, which is interpreted as you wanting to run the entire test
suite.  Also, the \verb!%! character and anything after is treated as
a comment.

\begin{code}
type Instruction = (FilePath, Maybe [String])

processInstructions :: Params -> IO Params
processInstructions config =
 do let is0 = case getFlagP TestSuiteFlg config of
              Just ts -> case getFlagP TestCaseFlg config of
                         Just c  -> [ (ts, Just [c]) ]
                         Nothing -> [ (ts, Nothing)  ]
              Nothing -> []
    is <- case getFlagP InstructionsFileFlg config of
            Nothing -> return []
            Just f  -> instructionsFile `fmap` readFile f
    -- basically set the test suite/case flag to the first instruction
    -- note that with the above code (which sets the first instruction
    -- to the test suite/case flag), this should work out to identity
    -- when those flags are provided.
    let instructions = is0 ++ is
        updateInstructions =
          setFlagP TestInstructionsFlg instructions
        updateTestCase =
          case (listToMaybe instructions >>= snd >>= listToMaybe) of
            Just c   -> setFlagP TestCaseFlg c
            Nothing  -> id
        updateTestSuite =
          case (fst `fmap` listToMaybe instructions) of
            Just s  -> setFlagP TestSuiteFlg s
            Nothing -> id
        updateFlags = updateInstructions . updateTestSuite . updateTestCase
    return $ updateFlags config

instructionsFile :: String -> [Instruction]
instructionsFile = mapMaybe inst . lines
 where
  inst l = case words (takeWhile (/= '%') l) of
           []     -> Nothing
           [f]    -> Just (f, Nothing)
           (f:cs) -> Just (f, Just cs)
\end{code}

% ====================================================================
% Flags
% ====================================================================

\begin{code}
{-
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

isFlag     :: (Typeable f, Typeable x) => (x -> f) -> Flag -> Bool
hasFlag    :: (Typeable f, Typeable x) => (x -> f) -> [Flag] -> Bool
deleteFlag :: (Typeable f, Typeable x) => (x -> f) -> [Flag] -> [Flag]
setFlag    :: (Eq f, Show f, Show x, Typeable f, Typeable x) => (x -> f) -> x -> [Flag] -> [Flag]
getFlag    :: (Show f, Show x, Typeable f, Typeable x)  => (x -> f) -> [Flag] -> Maybe x
getAllFlags :: (Show f, Show x, Typeable f, Typeable x) => (x -> f) -> [Flag] -> [x]

isFlag f1 (Flag f2 _) = typeOf f1 == typeOf f2
hasFlag f       = any (isFlag f)
deleteFlag f    = filter (not.(isFlag f))
setFlag f v fs  = (Flag f v) : tl where tl = deleteFlag f fs
getFlag f fs    = do (Flag _ v) <- find (isFlag f) fs ; cast v
getAllFlags f fs = catMaybes [ cast v | flg@(Flag _ v) <- fs, isFlag f flg ]


{-
Below are just the individual flags, which unfortunately have to be
defined as separate data types because of our fancy existential
data type code.
-}
-- input files
#define FLAG(x,y) data x = x y deriving (Eq, Show, Typeable)

FLAG (BatchDirFlg, FilePath)
FLAG (DisableGuiFlg, ())
FLAG (EarlyDeathFlg, ())
FLAG (ExtraPolaritiesFlg, (Map.Map String Interval))
FLAG (FromStdinFlg, ())
FLAG (HelpFlg, ())
FLAG (IgnoreSemanticsFlg, ())
FLAG (InstructionsFileFlg, FilePath)
FLAG (LexiconFlg, FilePath)
FLAG (MacrosFlg, FilePath)
FLAG (TracesFlg, FilePath)
FLAG (MaxTreesFlg, Int)
FLAG (MetricsFlg, [String])
FLAG (MorphCmdFlg, String)
FLAG (MorphInfoFlg, FilePath)
FLAG (MorphLexiconFlg, FilePath)
FLAG (OptimisationsFlg, [Optimisation])
FLAG (OutputFileFlg, String)
FLAG (PartialFlg, ())
FLAG (RegressionTestModeFlg, ())
FLAG (RootFeatureFlg, Flist)
FLAG (NoLoadTestSuiteFlg, ())
FLAG (StatsFileFlg, FilePath)
FLAG (TestCaseFlg, String)
FLAG (TestInstructionsFlg, [Instruction])
FLAG (TestSuiteFlg, FilePath)
FLAG (TimeoutFlg, Integer)
FLAG (VerboseModeFlg, ())
FLAG (ViewCmdFlg, String)
-- not to be exported (defaults)
-- the WeirdFlg exists strictly to please OS X when you launch
-- GenI in an application bundle (double-click)... for some
-- reason it wants to pass an argument to -p
FLAG (BuilderFlg,  BuilderType)
FLAG (GrammarTypeFlg, GrammarType)
FLAG (WeirdFlg, String)
\end{code}


