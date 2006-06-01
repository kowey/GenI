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
  ( Params(..), GrammarType(..), BuilderType(..), Switch(..)
  , mainBuilderTypes
  , polarised, predicting
  , rootcatfiltered, substfiltered, semfiltered,
  , isIaf
  , isBatch, emptyParams
  , treatArgs, treatArgsWithParams, optBatch
  )
where
\end{code}

\ignore{
\begin{code}
import qualified Data.Map as Map

import System.Console.GetOpt
import System.Exit ( exitWith, ExitCode(..) )
import Data.List  ( find, intersperse )
import Data.Maybe ( catMaybes  )
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
  -- tree selector (needed if xmgtools)
  selectCmd      :: String,
  -- | where to send XMG stderr chatter, stderr if null
  xmgErrFile :: FilePath,
  -- | where to dump the SelectTAG output (nowhere if null)
  xmgOutFile :: FilePath,
  -- tree viewer  (needed if xmgtools)
  viewCmd        :: String,
  --
  isGraphical    :: Bool,
  isServer       :: Bool,
  optimisations  :: [Switch],
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
  batchRepeat    :: Integer,
  --
  outputFile     :: String,
  -- statistical metricts
  metricsParam   :: [String],
  statsFile      :: FilePath,
  -- generation sans semantics (not the usual geni mode)
  ignoreSemantics :: Bool, 
  maxTrees       :: Maybe Int, -- limit on num of trees in a derived tree,
  -- timeout (s)
  timeoutSecs :: Maybe Integer
} deriving (Show)

hasOpt :: Switch -> Params -> Bool
hasOpt o p = o `elem` (optimisations p)

polarised, isIaf, predicting, isBatch :: Params -> Bool
rootcatfiltered, semfiltered, substfiltered :: Params -> Bool
polarised    = hasOpt PolarisedTok
isIaf        = hasOpt IafTok
predicting   = hasOpt PredictingTok
semfiltered  = hasOpt SemFilteredTok
substfiltered = hasOpt SubstFilteredTok
rootcatfiltered = hasOpt RootCatFilteredTok
isBatch      = hasOpt BatchTok
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
  selectCmd      = "runXMGselector",
  viewCmd        = "ViewTAG",
  isGraphical    = True,
  isServer       = False,
  testCase      = [],
  optimisations  = [],
  extrapol       = Map.empty,
  batchRepeat    = 1,
  outputFile     = "",
  metricsParam   = [],
  statsFile       = "",
  xmgOutFile      = "",
  xmgErrFile      = "",
  ignoreSemantics = False,
  maxTrees       = Nothing,
  timeoutSecs    = Nothing
}
\end{code}

\section{Parsing command line arguments}

\paragraph{options} We use the Haskell GetOpt library to process the
command line arguments.  To start things off, here is the list of command lines
switches that we use.  

\begin{code}
data GrammarType = GeniHand | TAGML | XMGTools
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
showBuilderType SimpleBuilder = "simple"
showBuilderType SimpleOnePhaseBuilder = "simple-1p"
showBuilderType CkyBuilder = "CKY"
showBuilderType EarleyBuilder = "Earley"

mainBuilderTypes :: [BuilderType]
mainBuilderTypes =
 [ SimpleBuilder, SimpleOnePhaseBuilder
 , CkyBuilder, EarleyBuilder]

data Switch = 
    HelpTok      |
    TestCasesTok String | TestSuiteTok String | 
    GraphicalTok Bool   | 
    CmdTok String String | -- key / command 
    OutputFileTok String |
    MetricsTok (Maybe String) | StatsFileTok String |
    XMGOutFileTok String | XMGErrFileTok String  |
    TimeoutTok String |
    IgnoreSemanticsTok Bool | MaxTreesTok String |
    BuilderTok String |
    -- grammar file
    GrammarType GrammarType  | 
    MacrosTok String         | LexiconTok String | MorphInfoTok String | 
    RootCategoriesTok String | 
    -- optimisations
    OptimisationsTok String   | PolOptsTok | AdjOptsTok |
    PolarisedTok | PredictingTok |
    ExtraPolaritiesTok String |
    RootCatFilteredTok | SubstFilteredTok | SemFilteredTok |
    IafTok {- one phase only! -} |
    BatchTok | RepeatTok String | 
    -- the WeirdTok exists strictly to please OS X when you launch
    -- GenI in an application bundle (double-click)... for some
    -- reason it wants to pass an argument to -p
    WeirdTok String 
    deriving (Show,Eq)
\end{code}

Here's the switches again and the switches they are associated with.
Note that we divide them into basic and advanced usage.

\begin{code}
options :: [OptDescr Switch]
options = optionsBasic ++ optionsAdvanced ++
  -- FIXME: weird mac stuff
  [ Option ['p']    []  (ReqArg WeirdTok "CMD") "" ]

optionsBasic :: [OptDescr Switch] 
optionsBasic =
  [ Option []    ["nogui"] (NoArg  (GraphicalTok False)) 
      "disable graphical user interface"
  , Option []    ["help"] (NoArg  HelpTok) 
      "show full list of command line switches"
  , Option ['m'] ["macros"] (ReqArg MacrosTok "FILE") 
      "macros file FILE (unanchored trees)"
  , Option ['l'] ["lexicon"] (ReqArg LexiconTok "FILE") 
      "lexicon file FILE"
  , Option ['s'] ["testsuite"] (ReqArg TestSuiteTok "FILE") 
      "test suite FILE"
  , Option []    ["rootcats"] (ReqArg RootCategoriesTok "LIST")
      ("root categories 'LIST' (for polarities, default:"
       ++ (unwords $ rootCatsParam emptyParams)
       ++ ")")
  , Option ['o'] ["output"] (ReqArg OutputFileTok "FILE")
      "output file FILE (stdout if unset)"
  , Option []    ["opts"] (ReqArg OptimisationsTok "LIST")
      "optimisations 'LIST' (--help for details)"
  ]
    
optionsAdvanced :: [OptDescr Switch] 
optionsAdvanced =
  [ Option ['b'] ["builder"]  (ReqArg BuilderTok "BUILDER")
      "use as realisation engine one of: simple cky"
  , Option []    ["timeout"] (ReqArg TimeoutTok "SECONDS")
      "time out after SECONDS seconds"
  , Option []    ["metrics"] (OptArg MetricsTok "LIST")
      "keep track of performance metrics: (default: iterations comparisons chart_size)"
  , Option []    ["statsfile"] (ReqArg StatsFileTok "FILE")
      "write performance data to file FILE (stdout if unset)"
  , Option []    ["xmgoutfile"] (ReqArg XMGOutFileTok "FILE")
      "write XMG anchoring results to file FILE; do not perform surface realisation"
  , Option []    ["xmgerrfile"] (ReqArg XMGErrFileTok "FILE")
      "write XMG anchoring stderr to file FILE (stderr if unset)"
  , Option []    ["xmgtools"] (NoArg (GrammarType XMGTools))
      "use XMG format for trees and GDE format for lexicon"
  , Option []    ["extrapols"] (ReqArg ExtraPolaritiesTok "STRING")
      "preset polarities (normally, you should use rootcats instead)" 
  , Option []    ["ignoresem"]   (NoArg (IgnoreSemanticsTok True))
      "ignore all semantic information"
  , Option []    ["maxtrees"]   (ReqArg MaxTreesTok "INT")
      "max tree size INT by number of elementary trees"
  , Option []    ["morphinfo"] (ReqArg MorphInfoTok "FILE")
      "morphological lexicon FILE (default: unset)"
  , Option []    ["morphcmd"]  (ReqArg (CmdTok "morph") "CMD") 
      "morphological post-processor CMD (default: unset)"
  , Option []    ["preselected"] (NoArg (GrammarType PreAnchored))
      "do NOT perform lexical selection - treat the grammar as the selection"
  , Option []    ["repeat"]   (ReqArg RepeatTok "INT")
      "perform INT trials during batch testing"
  , Option []    ["selectcmd"]  (ReqArg (CmdTok "select") "CMD") 
      "tree selecting/anchoring CMD (default: unset)"
  , Option []    ["testcase"]   (ReqArg TestCasesTok "String")
      "run test case STRING"
  , Option []    ["viewcmd"]  (ReqArg (CmdTok "view") "CMD") 
      "XMG tree-view command"
-- note: need to code optimisations string
  ]
\end{code}

\paragraph{optimisationCodes} In addition to the command line switches,
we have a lookup table of optimisation codes.  Each optimisation is
assigned a short codes like "a" for polarity detection.  This is useful 
both for taking command line arguments 
(something like \texttt{--opt=+pol+F}) and for telling the user in
concise form what optimisations she used.

\begin{code}
optimisationCodes :: [(Switch,String,String)]
optimisationCodes = 
 [ (PolarisedTok   , "p",      "polarity filtering")
 , (PolOptsTok  , "pol",    "equivalent to 'p'")
 , (AdjOptsTok  , "adj",    "equivalent to 'S F'")
 , (SemFilteredTok , "S",      "semantic filtering (same as f-sem)")
 , (SemFilteredTok , "f-sem",      "semantic filtering (two-phase only)")
 , (SubstFilteredTok, "f-sub",     "filtering on substitutions (two-phase only)")
 , (RootCatFilteredTok, "f-root",  "filtering on root node (two-phase only)")
 , (IafTok, "i", "index accesibility filtering (one-phase only)")
 , (BatchTok,          "batch", "batch processing") ]
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
        if HelpTok `elem` o 
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
  let polopts  = [PolOptsTok, PolarisedTok]
      adjopts  = [AdjOptsTok, SubstFilteredTok, RootCatFilteredTok, SemFilteredTok]
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
     ++ "Batch processing:\n"
     ++ "  " ++ (getstr BatchTok) ++ "\n"
\end{code}

\paragraph{parseOptimisations} parses a string of codes like \texttt{+pol+c}
into a list of optimisations.  We blithely ignore codes that we don't
recognise.

\begin{code}
parseOptimisations :: String -> [Switch] 
parseOptimisations str = 
  let codes = words str
  in  catMaybes (map lookupOptimisation codes)

lookupOptimisation :: String -> Maybe Switch
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
defineParams :: Params -> [Switch] -> Params
defineParams p [] = p
defineParams p (f:s) = defineParams pnext s
  where
    parsePol polStr =
      case runParser geniPolarities () "" polStr of
        Left err -> error (show err)
        Right p2 -> p2
    pnext = case f of 
      GraphicalTok v     -> p {isGraphical = v}
      OptimisationsTok v -> p {optimisations = readOpt v ++ (optimisations p)}
      OutputFileTok v    -> p {outputFile = v}
      -- grammar stuff
      MacrosTok    v -> p {macrosFile  = v}
      LexiconTok   v -> p {lexiconFile = v} 
      TestSuiteTok v -> p {tsFile = v}
      -- builders
      BuilderTok "null"   -> p { builderType = NullBuilder }
      BuilderTok "cky"    -> p { builderType = CkyBuilder }
      BuilderTok "earley" -> p { builderType = EarleyBuilder }
      BuilderTok "simple" -> p { builderType = SimpleBuilder }
      BuilderTok "simple-1p" -> p { builderType = SimpleOnePhaseBuilder }
      BuilderTok v        -> error ("unknown builder: " ++ v)
      -- advanced stuff
      RootCategoriesTok v -> p {rootCatsParam = words v}
      MorphInfoTok v      -> p {morphFile   = v}
      CmdTok "morph"    v -> p {morphCmd  = v}
      CmdTok "select"   v -> p {selectCmd = v}
      CmdTok "view"     v -> p {viewCmd = v}
      TestCasesTok v      -> p {testCase = v }
      -- performance profiling
      TimeoutTok v        -> p { timeoutSecs  = Just (read v) }
      MetricsTok Nothing  -> p { metricsParam = ["default"] }
      MetricsTok (Just v) -> p { metricsParam = words v }
      StatsFileTok v      -> p { statsFile    = v }
      XMGOutFileTok v     -> p { xmgOutFile   = v }
      XMGErrFileTok v     -> p { xmgErrFile   = v }
      --
      GrammarType v        -> p {grammarType = v} 
      IgnoreSemanticsTok v -> p { ignoreSemantics = v 
                                , maxTrees = case maxTrees p of
                                    Nothing  -> if v then Just 5 else Nothing 
                                    Just lim -> Just lim }
      MaxTreesTok v        -> p {maxTrees = Just (read v)} 
      ExtraPolaritiesTok v -> p {extrapol = parsePol v } 
      RepeatTok v          -> p {batchRepeat = read v}
      WeirdTok _           -> p
      unknown -> error ("Unknown configuration parameter: " ++ show unknown)
    -- when PolOpts and AdjOpts are in the list of optimisations
    -- then include all polarity-related optimisations and 
    -- all adjunction-related optimisations respectively
    readOpt v = addif PolOptsTok polOpts      
              $ addif AdjOptsTok adjOpts 
              $ parseOptimisations v
    addif t x o = if (t `elem` o) then x ++ o else o
    polOpts     = [PolarisedTok]
    adjOpts     = [SemFilteredTok]
\end{code}


\paragraph{optBatch} represents all meaningful combinations of optimisations
which include \fnparam{enabledRaw}.

\begin{code}
optBatch :: [Switch] -> [[Switch]] 
optBatch enabled =
  let use opt prev = if (opt `elem` enabled)
                     then withopt 
                     else withopt ++ prev
                     where withopt = map (opt:) prev
      -- 
      polBatch' = foldr use [[PolarisedTok]] []
      polBatch  = if PolarisedTok `elem` enabled
                 then polBatch' 
                 else [] : polBatch'
      adjBatch  = foldr use polBatch [SemFilteredTok]
      -- 
  in adjBatch
\end{code}

