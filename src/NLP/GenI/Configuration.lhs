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
{-# LANGUAGE ExistentialQuantification #-}
module NLP.GenI.Configuration
  ( Params(..)
  --
  , mainBuilderTypes
  , getFlagP, getListFlagP, setFlagP, hasFlagP, deleteFlagP, hasOpt
  , emptyParams, defineParams
  , treatArgs, treatArgsWithParams, usage, basicSections, optionsSections
  , processInstructions
  , optionsForStandardGenI
  , optionsForBasicStuff, optionsForOptimisation, optionsForMorphology, optionsForInputFiles
  , optionsForBuilder, optionsForTesting
  , nubBySwitches
  , noArg, reqArg, optArg
  , parseFlagWithParsec
  -- re-exports
  , module System.Console.GetOpt
  , module NLP.GenI.Flags
  , Typeable
  )
where
\end{code}

\ignore{
\begin{code}
import Control.Monad ( liftM )
import Data.Char ( toLower )
import Data.Maybe ( listToMaybe, mapMaybe )
import Data.Typeable ( Typeable )
import System.Console.GetOpt
import System.Environment ( getProgName )
import Data.List  ( find, intersperse, nubBy )
import Data.Maybe ( fromMaybe, isNothing, fromJust )
import Text.ParserCombinators.Parsec ( runParser, CharParser )

import NLP.GenI.Btypes ( showFlist, )
import NLP.GenI.Flags
import NLP.GenI.General ( geniBug, fst3, snd3 )
import NLP.GenI.GeniParsers ( geniFeats, geniPolarities )
import NLP.GenI.PolarityTypes ( readPolarityAttrs )
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
                  , Flag DetectPolaritiesFlg (readPolarityAttrs defaultPolarityAttrs)
                  ]
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
type OptSection = (String,[OptDescr Flag],[String])

-- | Uses the GetOpt library to process the command line arguments.
-- Note that we divide them into basic and advanced usage.
optionsForStandardGenI :: [OptDescr Flag]
optionsForStandardGenI =
  nubBySwitches $ concatMap snd3 optionsSections
                  ++ -- FIXME: weird mac stuff
                  [ Option ['p']    []  (reqArg WeirdFlg id "CMD") "" ]

basicSections :: [OptSection]
basicSections = map tweakBasic $ take 1 optionsSections
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
  example  = [ "Example:"
             , " geni -m examples/ej/mac -l examples/ej/lexicon -s examples/ej/suite"
             ]

getSwitches :: OptDescr a -> ([Char],[String])
getSwitches (Option s l _ _) = (s,l)

nubBySwitches :: [OptDescr a] -> [OptDescr a]
nubBySwitches = nubBy (\x y -> getSwitches x == getSwitches y)

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
\end{code}

\section{Options by theme}
\label{sec:fancy_parameters}

At the time of this writing (2009-09-25), it is highly unlikely that all the
options are documented here.  See \verb!geni --help!  for more details.

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

See Chapter \ref{cha:formats} for details on how to write these files.

\begin{description}
\item[macros]
  The \verb!macros! switch is used to supply GenI with FB-LTAG tree
  schemata.
\item[lexicon]
  The \verb!lexicon! is used for lexical entries that point to the
  macros
\item[suite]
  The \verb!suite! provides test cases on which to run GenI
\item[ranking]
  The \verb!ranking! switch allows you to specify a file containing
  Optimality Theory style constraints which GenI will use to rank
  its output.  See Chapter \ref{cha:ranking} for more details on the format
  and use of this file.
\end{description}

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
  , rankingOption
  , Option []    ["preselected"] (NoArg (Flag GrammarTypeFlg PreAnchored))
      "do NOT perform lexical selection - treat the grammar as the selection"
  ]

instructionsOption, macrosOption, lexiconOption, tracesOption :: OptDescr Flag

instructionsOption =
  Option [] ["instructions"] (reqArg InstructionsFileFlg id "FILE")
      "instructions file FILE"

macrosOption =
  Option ['m'] ["macros"] (reqArg MacrosFlg id "FILE")
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
\end{code}

% --------------------------------------------------------------------
\subsection{Output}
% --------------------------------------------------------------------

\begin{code}
optionsForOutput :: [OptDescr Flag]
optionsForOutput =
  [ outputOption
  , Option []    ["dump"]    (noArg DumpDerivationFlg)
      "print derivation information on stdout (JSON)"
  , Option []    ["partial"] (noArg PartialFlg)
      "return partial result(s) if no complete solution is found"
  -- same as rankingOption but with output-centric help text
  , Option [] ["ranking"] (reqArg RankingConstraintsFlg id "FILE")
    "use constraints in FILE to rank output"
  ]

outputOption :: OptDescr Flag
outputOption =
  Option ['o'] ["output"] (reqArg OutputFileFlg id "FILE")
    "output file FILE (stdout if unset)"
\end{code}

% --------------------------------------------------------------------
\subsection{User interface}
% --------------------------------------------------------------------

\begin{code}
optionsForUserInterface :: [OptDescr Flag]
optionsForUserInterface =
  [ noguiOption, helpOption, versionOption
  , Option []    ["regression"] (noArg RegressionTestModeFlg)
      "Run in regression testing mode (needs grammar, etc)"
  , Option []    ["unit-tests"] (noArg RunUnitTestFlg)
      "Run in unit testing mode (no arguments needed)"
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

\item[detect-pols]
  This tells GenI how to detect polarities in your grammar.  You pass
  this in in the form of a space-delimited string, where each word is either
  an attribute or a ``restricted'' attribute.  In lieu of an explanation,
  here is an example: the string ``cat idx V.tense D.c'' tells GenI that
  we should detect polarities on the ``cat'' and ``idx'' attribute
  for all nodes and also on the ``tense'' attribute for all nodes
  with the category ``V'' and the ``c'' attribute for all nodes with the
  category ``D''.

  If your grammar comes with its own hand-written polarities, you can
  suppress polarity detection altogether by supplying the empty string.

  Also, if you do not use this switch, the following defaults will be
  used:

\begin{includecodeinmanual}
\begin{code}
defaultPolarityAttrs :: String
defaultPolarityAttrs = "cat"
\end{code}
\end{includecodeinmanual}

\item[rootfeat]
  No results?  Make sure your rootfeat are set correctly.  GenI
  will reject all sentences whose root category does not unify
  with the rootfeat. A possible default root feature might be
\begin{includecodeinmanual}
\begin{code}
exampleRootFeat :: String
exampleRootFeat = "[cat:s inv:- mode:ind|subj wh:-]"
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
   , Option [] ["detect-pols"]
         (reqArg DetectPolaritiesFlg readPolarityAttrs "LIST")
         ("attributes 'LIST' (eg. \"cat idx V.tense\", default:" ++ show defaultPolarityAttrs ++ ")")
   , Option [] ["rootfeat"]
         (reqArg RootFeatureFlg readRF "FEATURE")
         ("root features 'FEATURE' (for polarities, example:"
          ++ showFlist exampleRF ++ ")")
  , Option [] ["extrapols"]
         (reqArg ExtraPolaritiesFlg readPolarities "STRING")
         "preset polarities (normally, you should use rootfeat instead)"
  ]
  where
   exampleRF = readRF exampleRootFeat
   readRF = parseFlagWithParsec "root feature" geniFeats
   readPolarities = parseFlagWithParsec "polarity string" geniPolarities

coreOptimisationCodes :: [(Optimisation,String,String)]
coreOptimisationCodes =
 [ (Polarised        , "p",      "polarity filtering")
 , (EarlyNa          , "e-na",   "detect null adjunction at earliest opportunity")
 , (SemFiltered      , "f-sem",  "semantic filtering (two-phase only)")
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
adjOpts = [EarlyNa, SemFiltered]
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
usageForOptimisations :: [String]
usageForOptimisations =
     [ "Optimisations must be passed in as a space-delimited list"
     , "(ex: --opt='p f-sem' for polarities and semantic filtering)"
     , ""
     , "Optimisations:"
     , "  " ++ unlinesTab (map describeOpt coreOptimisationCodes)
     ]
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
  A builder is basically a surface realisation algorithm.  \geni has the
  infrastructure to support different realisation algorithms, but some
  broken ones have been removed.
\end{description}

\begin{code}
optionsForBuilder :: [OptDescr Flag]
optionsForBuilder =
  [ Option ['b'] ["builder"]  (reqArg BuilderFlg readBuilderType "BUILDER")
      ("use as realisation engine one of: " ++ (unwords $ map show mainBuilderTypes))
  ]

mainBuilderTypes :: [BuilderType]
mainBuilderTypes =
 [ SimpleBuilder, SimpleOnePhaseBuilder
 ]

-- | Hint: compose with (map toLower) to make it case-insensitive
mReadBuilderType :: String -> Maybe BuilderType
mReadBuilderType "null"      = Just NullBuilder
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
  ]

morphInfoOption :: OptDescr Flag
morphInfoOption = Option [] ["morphinfo"] (reqArg MorphInfoFlg id "FILE")
  "morphological features FILE (default: unset)"
\end{code}

% ====================================================================
\section{Scripting GenI}
% ====================================================================

\begin{description}
\item[instructions] An instructions file can be used to run GenI on
a list of test suites and cases.

Any input that you give to GenI will be interpreted as a list of test
suites (and test cases that you want to run).  Each line has the format
\texttt{path/to/test-suite case1 case2 .. caseN}.   You can omit the
test cases, which is interpreted as you wanting to run the entire test
suite.  Also, the \verb!%! character and anything after is treated as
a comment.

Interaction with \verb!--testsuite! and \verb!--testcase!:
\begin{itemize}
\item If only \verb!--instructions! is set, then the first test suite
      and or test case from the instructions file is used.
\item If only \verb!--testsuite! and \verb!--testcase! are set, we
      pretend that an instructions file was supplied saying that we
      want to run the entirety of the test suite specified in
      \verb!--testsuite!.
\item If both \verb!--instructions! and \verb!--testsuite!/
      \verb!--testcase! are set then the latter are used to
      select from within the instructions.
\end{itemize}
\end{description}


\begin{code}
-- | Update the internal instructions list, test suite and case
--   according to the contents of an instructions file.
processInstructions :: Params -> IO Params
processInstructions config =
 do instructions <- case getFlagP InstructionsFileFlg config of
                      Nothing -> return fakeInstructions
                      Just f  -> instructionsFile `fmap` readFile f
    -- basically set the test suite/case flag to the first instruction
    -- note that with the above code (which sets the first instruction
    -- to the test suite/case flag), this should work out to identity
    -- when those flags are provided.
    let updateInstructions =
          setFlagP TestInstructionsFlg instructions
        updateTestCase p =
          if hasFlagP TestCaseFlg p then p
             else case (listToMaybe instructions >>= snd >>= listToMaybe) of
                   Just c   -> setFlagP TestCaseFlg c p 
                   Nothing  -> p
        updateTestSuite p =
          if hasFlagP TestSuiteFlg p then p
             else case (fst `fmap` listToMaybe instructions) of
                   Just s  -> setFlagP TestSuiteFlg s p
                   Nothing -> p
    return . updateInstructions . updateTestSuite . updateTestCase $ config
 where
  fakeInstructions =
     case getFlagP TestSuiteFlg config of
       Just ts -> [ (ts, Nothing) ]
       Nothing -> []

instructionsFile :: String -> [Instruction]
instructionsFile = mapMaybe inst . lines
 where
  inst l = case words (takeWhile (/= '%') l) of
           []     -> Nothing
           [f]    -> Just (f, Nothing)
           (f:cs) -> Just (f, Just cs)
\end{code}
