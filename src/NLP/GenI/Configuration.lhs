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

\chapter{Configuration}

This module handles configuration parameters from the command line.
The input to this module is simply \texttt{argv}.

\begin{code}
module NLP.GenI.Configuration
  ( Params(..), GrammarType(..), BuilderType(..), GeniFlag(..)
  , mainBuilderTypes
  , hasFlag, setFlag, hasOpt, polarised, predicting
  , rootcatfiltered, semfiltered
  , isIaf
  , emptyParams
  , treatArgs, treatArgsWithParams, optBatch
  )
where
\end{code}

\ignore{
\begin{code}
import qualified Data.Map as Map

import System.Console.GetOpt
import System.Exit ( exitWith, ExitCode(..) )
import Data.List  ( delete, find, intersperse )
import Data.Maybe ( catMaybes, fromMaybe  )
import Text.ParserCombinators.Parsec ( runParser )

import NLP.GenI.General ( geniBug, fst3, snd3, Interval )
import NLP.GenI.GeniParsers ( geniPolarities )
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
\section{Params}
% --------------------------------------------------------------------  

The Params data type holds the specification for how Geni should be
run, its input files, etc.  This is the stuff that would normally be
found in the configuration file. (FIXME move following comment?) There
are two basic generation modes in Geni: 
\begin{itemize}
\item one that does consider semantics (the original mode),
\item and one that does not consider semantics (addition jackie).
\end{itemize}
The purpose of the second option is to list (almost) all of the
sentences a grammar can produce, without bothering with semantics.
The generation includes some exceptions to ensure that Geni does not
infinitely loop.

\begin{code}
data Params = Prms{
  -- which generation engine to use 
  builderType    :: BuilderType,
  -- external morphological generator (optional)
  morphCmd       :: String,
  -- tree viewer  (useful if using an XMG grammar)
  viewCmd        :: String,
  --
  geniFlags      :: [GeniFlag],
  optimisations  :: [GeniFlag],
  --
  macrosFile     :: String,
  lexiconFile    :: String,
  tsFile         :: String, 
  morphFile      :: String,
  rootCatsParam  :: [String],
  grammarType    :: GrammarType,
  --
  testCase       :: String, -- names of test cases
  extrapol       :: Map.Map String Interval,
  batchDir       :: FilePath,
  --
  outputFile     :: String,
  -- statistical metricts
  metricsParam   :: [String],
  statsFile      :: FilePath,
  -- generation sans semantics (not the usual geni mode)
  maxTrees       :: Maybe Int, -- limit on num of trees in a derived tree,
  -- timeout (s)
  timeoutSecs :: Maybe Integer
} deriving (Show)

hasFlag :: GeniFlag -> Params -> Bool
hasFlag o p = o `elem` (geniFlags p)

hasOpt :: GeniFlag -> Params -> Bool
hasOpt o p = o `elem` (optimisations p)

polarised, isIaf, predicting :: Params -> Bool
rootcatfiltered, semfiltered :: Params -> Bool
polarised    = hasOpt PolarisedFlg
isIaf        = hasOpt IafFlg
predicting   = hasOpt PredictingFlg
semfiltered  = hasOpt SemFilteredFlg
rootcatfiltered = hasOpt RootCatFilteredFlg
\end{code}

\paragraph{defaultParams} returns the default parameters configuration

\begin{code}
emptyParams :: Params
emptyParams = Prms {
  builderType = SimpleBuilder,
  macrosFile  = "",
  lexiconFile = "",
  tsFile      = "",
  morphFile   = "",
  rootCatsParam = ["s"],
  grammarType   = GeniHand,
  morphCmd       = "",
  viewCmd        = "ViewTAG",
  testCase       = [],
  optimisations  = [],
  extrapol       = Map.empty,
  outputFile     = "",
  metricsParam   = [],
  statsFile       = "",
  batchDir        = "",
  geniFlags      = [EnableGuiFlg],
  maxTrees       = Nothing,
  timeoutSecs    = Nothing
}
\end{code}

\section{Parsing command line arguments}

\paragraph{options} We use the Haskell GetOpt library to process the
command line arguments.  To start things off, here is the list of command lines
switches that we use.  

\begin{code}
data GrammarType = GeniHand | TAGML
                 | PreCompiled -- ^ no parsing needed
                 | PreAnchored -- ^ lexical selection already done
     deriving (Show, Eq)

data BuilderType = NullBuilder |
                   SimpleBuilder | SimpleOnePhaseBuilder |
                   CkyBuilder | EarleyBuilder
     deriving (Eq)

instance Show BuilderType where
  show = showBuilderType

showBuilderType :: BuilderType -> String
showBuilderType NullBuilder = "null"
showBuilderType SimpleBuilder = "simple-2p"
showBuilderType SimpleOnePhaseBuilder = "simple-1p"
showBuilderType CkyBuilder = "CKY"
showBuilderType EarleyBuilder = "Earley"

mainBuilderTypes :: [BuilderType]
mainBuilderTypes =
 [ SimpleBuilder, SimpleOnePhaseBuilder
 , CkyBuilder, EarleyBuilder]

data GeniFlag = 
    HelpFlg      |
    TestCasesFlg String | TestSuiteFlg String | 
    EnableGuiFlg | DisableGuiFlag |
    CmdFlg String String | -- key / command 
    OutputFileFlg String |
    MetricsFlg (Maybe String) | StatsFileFlg String |
    XMGOutFileFlg String | XMGErrFileFlg String  |
    TimeoutFlg String |
    IgnoreSemanticsFlg | MaxTreesFlg String |
    BuilderFlg String |
    ServerModeFlg |
    -- grammar file
    GrammarType GrammarType  | 
    MacrosFlg String         | LexiconFlg String | MorphInfoFlg String | 
    RootCategoriesFlg String | 
    -- optimisations
    OptimisationsFlg String   | PolOptsFlg | AdjOptsFlg |
    PolarisedFlg | PredictingFlg | NoConstraintsFlg |
    ExtraPolaritiesFlg String |
    RootCatFilteredFlg | SemFilteredFlg |
    IafFlg {- one phase only! -} |
    BatchDirFlg FilePath | RepeatFlg String |
    -- the WeirdFlg exists strictly to please OS X when you launch
    -- GenI in an application bundle (double-click)... for some
    -- reason it wants to pass an argument to -p
    WeirdFlg String 
    deriving (Show,Eq)
\end{code}

Here's the switches again and the switches they are associated with.
Note that we divide them into basic and advanced usage.

\begin{code}
options :: [OptDescr GeniFlag]
options = optionsBasic ++ optionsAdvanced ++
  -- FIXME: weird mac stuff
  [ Option ['p']    []  (ReqArg WeirdFlg "CMD") "" ]

optionsBasic :: [OptDescr GeniFlag] 
optionsBasic =
  [ Option []    ["nogui"] (NoArg DisableGuiFlag)
      "disable graphical user interface"
  , Option []    ["help"] (NoArg  HelpFlg) 
      "show full list of command line switches"
  , Option ['m'] ["macros"] (ReqArg MacrosFlg "FILE") 
      "macros file FILE (unanchored trees)"
  , Option ['l'] ["lexicon"] (ReqArg LexiconFlg "FILE") 
      "lexicon file FILE"
  , Option ['s'] ["testsuite"] (ReqArg TestSuiteFlg "FILE") 
      "test suite FILE"
  , Option []    ["rootcats"] (ReqArg RootCategoriesFlg "LIST")
      ("root categories 'LIST' (for polarities, default:"
       ++ (unwords $ rootCatsParam emptyParams)
       ++ ")")
  , Option ['o'] ["output"] (ReqArg OutputFileFlg "FILE")
      "output file FILE (stdout if unset)"
  , Option []    ["opts"] (ReqArg OptimisationsFlg "LIST")
      "optimisations 'LIST' (--help for details)"
  ]
    
optionsAdvanced :: [OptDescr GeniFlag] 
optionsAdvanced =
  [ Option ['b'] ["builder"]  (ReqArg BuilderFlg "BUILDER")
      "use as realisation engine one of: simple cky"
  , Option []    ["timeout"] (ReqArg TimeoutFlg "SECONDS")
      "time out after SECONDS seconds"
  , Option []    ["metrics"] (OptArg MetricsFlg "LIST")
      "keep track of performance metrics: (default: iterations comparisons chart_size)"
  , Option []    ["statsfile"] (ReqArg StatsFileFlg "FILE")
      "write performance data to file FILE (stdout if unset)"
  , Option []    ["extrapols"] (ReqArg ExtraPolaritiesFlg "STRING")
      "preset polarities (normally, you should use rootcats instead)" 
  , Option []    ["ignoresem"]   (NoArg IgnoreSemanticsFlg)
      "ignore all semantic information"
  , Option []    ["maxtrees"]   (ReqArg MaxTreesFlg "INT")
      "max tree size INT by number of elementary trees"
  , Option []    ["morphinfo"] (ReqArg MorphInfoFlg "FILE")
      "morphological lexicon FILE (default: unset)"
  , Option []    ["morphcmd"]  (ReqArg (CmdFlg "morph") "CMD") 
      "morphological post-processor CMD (default: unset)"
  , Option []    ["preselected"] (NoArg (GrammarType PreAnchored))
      "do NOT perform lexical selection - treat the grammar as the selection"
  , Option []    ["batchdir"]    (ReqArg BatchDirFlg "DIR")
      "batch process the test suite and save results to DIR"
  , Option []    ["testcase"]   (ReqArg TestCasesFlg "String")
      "run test case STRING"
  , Option []    ["viewcmd"]  (ReqArg (CmdFlg "view") "CMD") 
      "XMG tree-view command"
-- note: need to code optimisations string
  ]
\end{code}

\paragraph{optimisationCodes} In addition to the command line switches,
we have a lookup table of optimisation codes.  Each optimisation is
assigned a short codes like "a" for polarity detection.  This is useful 
both for taking command line arguments 
(something like \texttt{--opt='pol S i'}) and for telling the user in
concise form what optimisations she used.

\begin{code}
optimisationCodes :: [(GeniFlag,String,String)]
optimisationCodes = 
 [ (PolarisedFlg   , "p",      "polarity filtering")
 , (PolOptsFlg  , "pol",    "equivalent to 'p'")
 , (AdjOptsFlg  , "adj",    "equivalent to 'S F'")
 , (SemFilteredFlg , "S",      "semantic filtering (same as f-sem)")
 , (SemFilteredFlg , "f-sem",      "semantic filtering (two-phase only)")
 , (RootCatFilteredFlg, "f-root",  "filtering on root node (two-phase only)")
 , (IafFlg, "i", "index accesibility filtering (one-phase only)")
 , (NoConstraintsFlg, "nc", "disable semantic constraints (anti-optimisation!)")
 ]
\end{code}

\paragraph{treatArgs} does the actual work of parsing command line arguments 
(represented as a list of strings).   

\begin{code}
treatArgs :: [String] -> IO Params
treatArgs argv = treatArgsWithParams argv emptyParams

treatArgsWithParams :: [String] -> Params -> IO Params
treatArgsWithParams argv initParams = do
   let header   = "Usage: geni [OPTION...]"
       usageExample = "Example:\n" ++  
         " geni --gui -m examples/ej/mac -l examples/ej/lexicon -s examples/ej/suite\n"
       usage    = usageInfo header optionsBasic ++ "\n\n" ++usageExample
       usageAdv = usage 
                  ++ usageInfo "Advanced options (note: all LIST are space delimited)" optionsAdvanced
                  ++ optimisationsUsage
                  ++ "\n\n" ++ usageExample
   case getOpt Permute options argv of
     (o,_,[]  ) -> 
        if HelpFlg `elem` o 
             then do putStrLn usageAdv
                     exitWith ExitSuccess
             else return (defineParams initParams o)
     (_,_,errs) -> ioError (userError $ concat errs ++ usage)
\end{code}

\paragraph{optimisationsUsage} displays the usage text for optimisations.  
It shows a table of optimisation codes and their meaning.

\begin{code}
optimisationsUsage :: String
optimisationsUsage = 
  let polopts  = [PolOptsFlg, PolarisedFlg]
      adjopts  = [AdjOptsFlg, RootCatFilteredFlg, SemFilteredFlg]
      unlinesTab l = concat (intersperse "\n  " l)
      getstr k = case find (\x -> k == fst3 x) optimisationCodes of 
                   Just (_, code, desc) -> code ++ " - " ++ desc
                   Nothing -> geniBug $ "code" ++ show k ++ "not found in optimisationsUsage"
  in "\n" 
     ++ "List of optimisations.\n"
     ++ "(ex: --opt='f s' for foot constraints and semantic filters)\n"
     ++ "\n"
     ++ "Polarity optimisations:\n"
     ++ "  " ++ unlinesTab (map getstr polopts) ++ "\n\n"
     ++ "Adjunction optimisations:\n"
     ++ "  " ++ unlinesTab (map getstr adjopts) ++ "\n"
\end{code}

\paragraph{parseOptimisations} parses a string of codes like \texttt{+pol+c}
into a list of optimisations.  We blithely ignore codes that we don't
recognise.

\begin{code}
parseOptimisations :: String -> [GeniFlag] 
parseOptimisations str = 
  let codes = words str
  in  catMaybes (map lookupOptimisation codes)

lookupOptimisation :: String -> Maybe GeniFlag
lookupOptimisation code = do
  triple <- find (\x -> snd3 x == code) optimisationCodes
  return (fst3 triple)
\end{code}

% --------------------------------------------------------------------  
\section{Values for Params}
% --------------------------------------------------------------------  

The configuration file is intepreted as a list of lists of tokens.  We
use a list of lists for purposes of batch processing.  Each list of
tokens is a session.  Each session inherits the properties of the
previous sesssion, except for the optimisations  

If there is no batch processing; then we only have
a singleton list of lists.  

\paragraph{defineParams} Rewrites the configuration in Params using L, where 
\begin{itemize}
\item a Params structure p (previous parameters)
\item a list L of lists of pairs (Variable, Value) 
\end{itemize}

\begin{code}
defineParams :: Params -> [GeniFlag] -> Params
defineParams p [] = p
defineParams p (f:s) = defineParams pnext s
  where
    parsePol polStr =
      case runParser geniPolarities () "" polStr of
        Left err -> error (show err)
        Right p2 -> p2
    pnext = case f of 
      DisableGuiFlag     -> deleteFlag EnableGuiFlg p
      OptimisationsFlg v -> p {optimisations = readOpt v ++ (optimisations p)}
      OutputFileFlg v    -> p {outputFile = v}
      -- grammar stuff
      MacrosFlg    v -> p {macrosFile  = v}
      LexiconFlg   v -> p {lexiconFile = v} 
      TestSuiteFlg v -> p {tsFile = v}
      -- builders
      BuilderFlg "null"   -> p { builderType = NullBuilder }
      BuilderFlg "cky"    -> p { builderType = CkyBuilder }
      BuilderFlg "earley" -> p { builderType = EarleyBuilder }
      BuilderFlg "simple" -> p { builderType = SimpleBuilder }
      BuilderFlg "simple-2p" -> p { builderType = SimpleBuilder }
      BuilderFlg "simple-1p" -> p { builderType = SimpleOnePhaseBuilder }
      BuilderFlg v        -> error ("unknown builder: " ++ v)
      -- advanced stuff
      RootCategoriesFlg v -> p {rootCatsParam = words v}
      MorphInfoFlg v      -> p {morphFile   = v}
      CmdFlg "morph"    v -> p {morphCmd  = v}
      CmdFlg "view"     v -> p {viewCmd = v}
      TestCasesFlg v      -> p {testCase = v }
      -- performance profiling
      TimeoutFlg v        -> p { timeoutSecs  = Just (read v) }
      MetricsFlg Nothing  -> p { metricsParam = ["default"] }
      MetricsFlg (Just v) -> p { metricsParam = words v }
      StatsFileFlg v      -> p { statsFile    = v }
      --
      GrammarType v        -> p {grammarType = v} 
      IgnoreSemanticsFlg   -> addFlag IgnoreSemanticsFlg $
                                p { maxTrees = Just $ fromMaybe 5 $ maxTrees p }
      MaxTreesFlg v        -> p {maxTrees = Just (read v)} 
      ExtraPolaritiesFlg v -> p {extrapol = parsePol v } 
      BatchDirFlg  v       -> p {batchDir = v}
      WeirdFlg _           -> p
      unknown -> error ("Unknown configuration parameter: " ++ show unknown)
    -- when PolOpts and AdjOpts are in the list of optimisations
    -- then include all polarity-related optimisations and 
    -- all adjunction-related optimisations respectively
    readOpt v = addif PolOptsFlg polOpts      
              $ addif AdjOptsFlg adjOpts 
              $ parseOptimisations v
    addif t x o = if (t `elem` o) then x ++ o else o
    polOpts     = [PolarisedFlg]
    adjOpts     = [SemFilteredFlg]

addFlag :: GeniFlag -> Params -> Params
addFlag f p = p { geniFlags = f : (geniFlags p) }

setFlag :: GeniFlag -> Bool -> Params -> Params
setFlag f b pRaw = if b then addFlag f p else p
  where p = deleteFlag f pRaw

deleteFlag :: GeniFlag -> Params -> Params
deleteFlag f p = p { geniFlags = delete f (geniFlags p) }
\end{code}


\paragraph{optBatch} represents all meaningful combinations of optimisations
which include \fnparam{enabledRaw}.

\begin{code}
optBatch :: [GeniFlag] -> [[GeniFlag]] 
optBatch enabled =
  let use opt prev = if (opt `elem` enabled)
                     then withopt 
                     else withopt ++ prev
                     where withopt = map (opt:) prev
      -- 
      polBatch' = foldr use [[PolarisedFlg]] []
      polBatch  = if PolarisedFlg `elem` enabled
                 then polBatch' 
                 else [] : polBatch'
      adjBatch  = foldr use polBatch [SemFilteredFlg]
      -- 
  in adjBatch
\end{code}

