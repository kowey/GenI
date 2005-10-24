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
module Configuration 
  (Params(..), GrammarType(..), Switch(..),
   autopol, polarised, polsig, predicting,
   semfiltered, chartsharing, footconstr, 
   isBatch, emptyParams, 
   treatArgs, optBatch)
where
\end{code}

\ignore{
\begin{code}
import qualified Data.Map as Map

import System.Console.GetOpt
import System.Exit ( exitWith, ExitCode(..) )
import Data.List  ( find, intersperse )
import Data.Maybe ( catMaybes  )
import General ( bugInGeni, fst3, snd3, wordsBy ) 
import GeniParsers (parsePol)
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
  -- external morphological generator (optional)
  morphCmd       :: String,
  -- tree selector (needed if xmgtools)
  selectCmd      :: String,
  -- tree viewer  (needed if xmgtools)
  viewCmd        :: String,
  --
  isGraphical    :: Bool,
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
  extrapol       :: Map.Map String Int,
  batchRepeat    :: Integer,
  usetrash       :: Bool,
  --
  outputFile     :: String,
  -- generation sans semantics (not the usual geni mode)
  ignoreSemantics :: Bool, 
  maxTrees       :: Maybe Int -- limit on num of trees in a derived tree 
} deriving (Show)

autopol      :: Params -> Bool
polarised    :: Params -> Bool
polsig       :: Params -> Bool
predicting   :: Params -> Bool
semfiltered  :: Params -> Bool
chartsharing :: Params -> Bool
footconstr   :: Params -> Bool
isBatch      :: Params -> Bool

autopol      p = AutoPolTok      `elem` (optimisations p)
polarised    p = PolarisedTok    `elem` (optimisations p)
polsig       p = PolSigTok       `elem` (optimisations p)
predicting   p = PredictingTok   `elem` (optimisations p)  
semfiltered  p = SemFilteredTok  `elem` (optimisations p)
chartsharing p = ChartSharingTok `elem` (optimisations p)
footconstr   p = FootConstraintTok `elem` (optimisations p)
isBatch      p = BatchTok          `elem` (optimisations p)
\end{code}

\paragraph{defaultParams} returns the default parameters configuration

\begin{code}
emptyParams :: Params
emptyParams = Prms {
  macrosFile  = "",
  lexiconFile = "",
  tsFile      = "",
  morphFile   = "",
  rootCatsParam = ["s"],
  grammarType   = GeniHand,
  morphCmd       = "",
  selectCmd      = "runXMGselector.sh",
  viewCmd        = "ViewTAG",
  isGraphical    = True,
  testCase      = [],
  optimisations  = [],
  extrapol       = Map.empty,
  batchRepeat    = 1,
  usetrash       = False,
  outputFile     = "",
  ignoreSemantics = False,
  maxTrees       = Nothing
}
\end{code}

\section{Parsing command line arguments}

\paragraph{options} We use the Haskell GetOpt library to process the
command line arguments.  To start things off, here is the list of command lines
switches that we use.  

\begin{code}
data GrammarType = GeniHand | TAGML | XMGTools 
     deriving (Show, Eq)

data Switch = 
    HelpTok      |
    TestCasesTok String | TestSuiteTok String | 
    GraphicalTok Bool   | 
    CmdTok String String | -- key / command 
    OutputFileTok String |
    IgnoreSemanticsTok Bool | MaxTreesTok String |
    -- grammar file
    GrammarType GrammarType  | 
    MacrosTok String         | LexiconTok String | MorphInfoTok String | 
    RootCategoriesTok String | 
    -- optimisations
    OptimisationsTok String   | PolOptsTok | AdjOptsTok |
    PolarisedTok | AutoPolTok | PolSigTok  | PredictingTok | ChartSharingTok |
    ExtraPolaritiesTok String |
    FootConstraintTok         | SemFilteredTok | OrderedAdjTok |  
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
  , Option ['o'] ["output"] (ReqArg OutputFileTok "FILE")
      "output file FILE (stdout if unset)"
  , Option []    ["opts"] (ReqArg OptimisationsTok "LIST")
      "optimisations LIST (--help for details)"
  ]
    
optionsAdvanced :: [OptDescr Switch] 
optionsAdvanced =
  [ Option []    ["xmgtools"] (NoArg (GrammarType XMGTools))
      "use XMG format for trees and GDE format for lexicon"
  , Option []    ["extrapols"] (ReqArg ExtraPolaritiesTok "LIST")
      "preset polarities (normally, you should use rootcats instead)" 
  , Option []    ["ignoresem"]   (NoArg (IgnoreSemanticsTok True))
      "ignore all semantic information"
  , Option []    ["maxtrees"]   (ReqArg MaxTreesTok "INT")
      "max tree size INT by number of elementary trees"
  , Option []    ["morphinfo"] (ReqArg MorphInfoTok "FILE")
      "morphological lexicon FILE (default: unset)"
  , Option []    ["morphcmd"]  (ReqArg (CmdTok "morph") "CMD") 
      "morphological post-processor CMD (default: unset)"
  , Option []    ["repeat"]   (ReqArg RepeatTok "INT")
      "perform INT trials during batch testing"
  , Option []    ["rootcats"] (ReqArg RootCategoriesTok "LIST")
      ("root categories LIST (for polarities, default:" 
       ++ (concat $ intersperse "+" $ rootCatsParam emptyParams) 
       ++ ")")
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
 , (PolOptsTok  , "pol",    "equivalent to +p+a+s+c")
 , (AdjOptsTok  , "adj",    "equivalent to +S+F")
 , (AutoPolTok     , "a",      "polarity detection")
 , (PolSigTok      , "s",      "polarity signatures")
 , (ChartSharingTok, "c",      "chart sharing")
 , (SemFilteredTok , "S",      "semantic filtering")
 , (OrderedAdjTok  , "O",      "ordered adjunction (by node)")
 , (FootConstraintTok,    "F", "foot constraints")
 , (BatchTok,          "batch", "batch processing") ]
\end{code}

\paragraph{treatArgs} does the actual work of parsing command line arguments 
(represented as a list of strings).   

\begin{code}
treatArgs :: [String] -> IO Params
treatArgs argv = do
   let header   = "Usage: geni [OPTION...]"
       usageExample = "Example:\n" ++  
         " geni --gui -m examples/ej/mac -l examples/ej/lexicon -s examples/ej/suite\n"
       usage    = usageInfo header optionsBasic ++ "\n\n" ++usageExample
       usageAdv = usage 
                  ++ usageInfo "Advanced options (note: all LIST are + delimited)" optionsAdvanced 
                  ++ optimisationsUsage
                  ++ "\n\n" ++ usageExample
   case getOpt Permute options argv of
     (o,_,[]  ) -> 
        if HelpTok `elem` o 
             then do putStrLn usageAdv
                     exitWith ExitSuccess
             else return (defineParams emptyParams o)
     (_,_,errs) -> ioError (userError $ concat errs ++ usage)
\end{code}

\paragraph{optimisationsUsage} displays the usage text for optimisations.  
It shows a table of optimisation codes and their meaning.

\begin{code}
optimisationsUsage :: String
optimisationsUsage = 
  let polopts  = [PolOptsTok, PolarisedTok, AutoPolTok, PolSigTok, ChartSharingTok]
      adjopts  = [AdjOptsTok, SemFilteredTok, FootConstraintTok]
      unlinesTab l = concat (intersperse "\n  " l)
      getstr k = case find (\x -> k == fst3 x) optimisationCodes of 
                   Just (_, code, desc) -> code ++ " - " ++ desc
                   Nothing -> error ("code" ++ show k ++ "not found in optimisationsUsage\n" ++ bugInGeni)
  in "\n" 
     ++ "List of optimisations.\n"
     ++ "(ex: --opt=+f+s for foot constraints and semantic filters)\n"
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
  let codes = wordsBy '+' str 
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
  where pnext = case f of 
            GraphicalTok v     -> p {isGraphical = v}
            OptimisationsTok v -> p {optimisations = readOpt v } 
            OutputFileTok v    -> p {outputFile = v}
            -- grammar stuff
            MacrosTok    v -> p {macrosFile  = v}
            LexiconTok   v -> p {lexiconFile = v} 
            TestSuiteTok v -> p {tsFile = v}
            -- advanced stuff
            RootCategoriesTok v -> p {rootCatsParam = wordsBy '+' v}
            MorphInfoTok v      -> p {morphFile   = v}
            CmdTok "morph"    v -> p {morphCmd  = v}
            CmdTok "select"   v -> p {selectCmd = v}
            CmdTok "view"     v -> p {viewCmd = v}
            TestCasesTok v      -> p {testCase = v }
            -- 
            GrammarType v        -> p {grammarType = v} 
            IgnoreSemanticsTok v -> p { ignoreSemantics = v 
                                      , maxTrees = case maxTrees p of
                                          Nothing  -> if v then Just 5 else Nothing 
                                          Just lim -> Just lim }
            MaxTreesTok v        -> p {maxTrees = Just (read v)} 
            ExtraPolaritiesTok v -> p {extrapol = parsePol v} 
            RepeatTok v          -> p {batchRepeat = read v}
            WeirdTok _           -> p
            p -> error ("Unknown configuration parameter: " ++ show p)
        -- when PolOpts and AdjOpts are in the list of optimisations
        -- then include all polarity-related optimisations and 
        -- all adjunction-related optimisations respectively
        readOpt v = addif PolOptsTok polOpts      
                  $ addif AdjOptsTok adjOpts 
                  $ parseOptimisations v
        addif t x o = if (t `elem` o) then x ++ o else o
        polOpts     = [PolarisedTok, AutoPolTok, ChartSharingTok] 
        adjOpts     = [SemFilteredTok, FootConstraintTok]
\end{code}


\paragraph{optBatch} represents all meaningful combinations of optimisations
which include \fnparam{enabledRaw}.  By meaningful combination, for example, we
not have a combination that has polarity signatures, but not polarities.

\begin{code}
optBatch :: [Switch] -> [[Switch]] 
optBatch enabledRaw = 
  let enabled = if (ChartSharingTok `elem` enabledRaw || PolSigTok `elem` enabledRaw) 
                then PolarisedTok:enabledRaw
                else enabledRaw
      use opt prev = if (opt `elem` enabled) 
                     then withopt 
                     else withopt ++ prev
                     where withopt = map (opt:) prev
      -- 
      polBatch' = foldr use [[PolarisedTok]] [AutoPolTok,ChartSharingTok]
      polBatch  = if PolarisedTok `elem` enabled
                 then polBatch' 
                 else [] : polBatch'
      adjBatch  = foldr use polBatch [SemFilteredTok,FootConstraintTok]
      -- 
  in adjBatch
\end{code}

