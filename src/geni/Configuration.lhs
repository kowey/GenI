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

This module handles configuration parameters such as the input files and
the optimisations that Geni should handle.  The input to this module
comes from Cparser.y

\textbf{TODO}:
\begin{enumerate}
\item change name functions in of Params to xFile
\end{enumerate}

\begin{code}
module Configuration(
   Params, 
   treatArgs, 
   grammarFile, isGraphical,
   morphCmd, testCases,
   optimisations,
   autopol, polarised, polsig, chartsharing, extrapol,
   predicting, semfiltered, footconstr,
   isBatch, batchRepeat, usetrash,
   defaultParams, emptyParams, getConf, optBatch,
   emptyGramParams, ignoreSemantics, maxTrees,

   GramParams(..),
   GrammarType(..),
   parseGramIndex,

   -- re-export
   Token(Batch)
)

where
\end{code}

\ignore{
\begin{code}
import Data.List (intersperse)
import qualified Data.Map as Map

import GeniParsers (Token(..), E(..), parseConfig, parseIndex, parsePol)
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
\section{Configuration}
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
  grammarFile    :: String,
  morphCmd       :: String,
  isGraphical    :: Bool,
  optimisations  :: [Token],
  testCases      :: [String], -- names of test cases
  extrapol       :: Map.Map String Int,
  batchRepeat    :: Integer,
  usetrash       :: Bool,
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

autopol      p = AutoPol      `elem` (optimisations p)
polarised    p = Polarised    `elem` (optimisations p)
polsig       p = PolSig       `elem` (optimisations p)
predicting   p = Predicting   `elem` (optimisations p)  
semfiltered  p = SemFiltered  `elem` (optimisations p)
chartsharing p = ChartSharing `elem` (optimisations p)
footconstr   p = FootConstraint `elem` (optimisations p)
isBatch      p = Batch          `elem` (optimisations p)
\end{code}

\paragraph{defaultParams} returns the default parameters configuration

\begin{code}
emptyParams :: Params
emptyParams = Prms {
  grammarFile    = "",
  morphCmd       = "",
  isGraphical    = False,
  testCases      = [],
  optimisations  = [],
  extrapol       = Map.empty,
  batchRepeat    = 1,
  usetrash       = False,
  ignoreSemantics = False,
  maxTrees       = Nothing
}

defaultParams :: Params
defaultParams = emptyParams {
   grammarFile    = "examples/ej/index",
   isGraphical    = True
}
\end{code}

\paragraph{getConf} reads file .genirc for configuration if it exists,
otherwise it creates the file with default values and warns 
the user. 

\begin{code}
getConf :: Params -> IO [Params]
getConf p =
  catch getConf' (\_ -> createConf)
      where getConf' = do fconf <- readFile ".genirc"
                          case parseConfig fconf of 
                            Ok x     -> return (defineParams p x)
                            Failed x -> fail x 
            createConf = do writeFile ".genirc" (defaultParamsStr p)
                            putStr "Looks like the first time you are running GenI\n"
                            putStr "Writing default configuration file in .genirc.\n"
                            return [p]
\end{code}

\paragraph{treatArgs} Parses L and updates p accordingly, with
\begin{itemize}
\item some Param structures p of default values
\item a list of strings L read from the comand line
\end{itemize}

Note: we treat the arguments as a continuation of the .genirc file
and nothing more.

\begin{code}
treatArgs :: [Params] -> [String] -> [Params]
treatArgs params s =
  case (parseConfig . unwords) s of
   Ok x     -> params ++ defineParams (last params) x
   Failed x -> error x
\end{code}


\paragraph{defaultParamsStr} given
  - a Params structure with the default values
returns a string that is used to generate the .genirc default
configuration file
\begin{code}
defaultParamsStr :: Params -> String
defaultParamsStr p = 
  let g  = grammarFile p
      op = optimisations p
      gr = if (isGraphical p)  then "True" else "False"
  in "\nGrammar  = " ++ g  ++ 
     "\n" ++
     "\n% True or False" ++
     "\nGraphical  = " ++ gr ++ 
     "\n" ++
     "\n% Optimisations should be a comma delimited list containing any " ++
     "\n% number of the following items:" ++
     "\n%  Polarised, PolSig, ChartSharing," ++
     "\n%  SemFiltered, FootConstraint" ++
     "\n%  There is also PolOpts (all polarity optimisations)" ++ 
     "\n%  and AdjOpts (all adjunction optimisations)"  ++
     "\nOptimisations = " ++ 
     "\n" ++ (concat $ intersperse "," $ map show op) 
\end{code}

% --------------------------------------------------------------------  
\subsection{Intepreting commands}
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
defineParams :: Params -> [[(Token,String)]] -> [ Params ]
defineParams _ []        = []
defineParams p (fv:next) = nextP : (defineParams nextP next)
  where nextP = defineParams' p fv

defineParams' :: Params -> [(Token,String)] -> Params
defineParams' p [] = p
defineParams' p ((f,v):s) = defineParams' pnext s
  where pnext = case f of 
            GrammarTok      -> p {grammarFile = v}
            MorphCmdTok     -> p {morphCmd = v}
            TestCasesTok    -> p {testCases = words v }
            GraphicalTok    -> p {isGraphical = (v == "True")}
            IgnoreSemanticsTok -> p { ignoreSemantics = (v == "True")
                                    , maxTrees = case maxTrees p of
                                          Nothing  -> if (v == "True") then Just 5
                                                      else Nothing 
                                          Just lim -> Just lim }
            MaxTreesTok     -> p {maxTrees = Just (read v)} 
            Optimisations   -> p {optimisations = readOpt } 
            ExtraPolarities -> p {extrapol = parsePol v} 
            Repeat          -> p {batchRepeat = read v}
            p -> error ("Unknown configuration parameter: " ++ show p)
        -- when PolOpts and AdjOpts are in the list of optimisations
        -- then include all polarity-related optimisations and 
        -- all adjunction-related optimisations respectively
        readOpt = (addif PolOptsTok polOpts) 
                  $ (addif AdjOptsTok adjOpts) $ (map read $ words v)
        addif t x o = if (t `elem` o) then x ++ o else o
        polOpts     = [Polarised, AutoPol, ChartSharing] 
        adjOpts     = [SemFiltered, FootConstraint]
\end{code}


\paragraph{optBatch} represents all meaningful combinations of optimisations
which include \fnparam{enabledRaw}.  By meaningful combination, for example, we
not have a combination that has polarity signatures, but not polarities.

\begin{code}
optBatch :: [Token] -> [[Token]] 
optBatch enabledRaw = 
  let enabled = if (ChartSharing `elem` enabledRaw || PolSig `elem` enabledRaw) 
                then Polarised:enabledRaw
                else enabledRaw
      use opt prev = if (opt `elem` enabled) 
                     then withopt 
                     else withopt ++ prev
                     where withopt = map (opt:) prev
      -- 
      polBatch' = foldr use [[Polarised]] [AutoPol,ChartSharing]
      polBatch  = if Polarised `elem` enabled
                 then polBatch' 
                 else [] : polBatch'
      adjBatch  = foldr use polBatch [SemFiltered,FootConstraint]
      -- 
  in adjBatch
\end{code}

% --------------------------------------------------------------------  
\section{Reading Grammar Params}
% --------------------------------------------------------------------  
 
All grammars have an index file which contains information about the
grammar type and refers to the other files in the grammar.  The functions
in this section interpret the contents of the grammar index file.

\begin{code}
data GrammarType = GeniHand | TAGML | CGManifesto 
     deriving (Show, Eq)
\end{code}

The GramParams data type holds the information about a single
GenI grammar

\begin{code}
data GramParams = GrmPrms {
  macrosFile     :: String,
  lexiconFile    :: String,
  lexiconDir     :: String,
  tsFile         :: String, 
  morphFile      :: String,
  rootCatsParam  :: [String],
  grammarType    :: GrammarType
} deriving (Show)

emptyGramParams = defineGramParams []
\end{code}

\paragraph{parseGramIndex} Parses the contents of a grammar index
file.  All relative filenames in the index are prepended with the
parent directory of \texttt{filename} 
(i.e. if i pass in \texttt{examples/foo/index} as filename, then 
 a filename like \texttt{lexicon} will be converted to
 \texttt{examples/foo/lexicon}).

\begin{code}
parseGramIndex :: FilePath -> String -> GramParams
parseGramIndex filename contents =
  let slash = '/'
      -- parse the index file 
      args = case parseIndex contents of
               Ok x     -> x 
               Failed x -> error x
      gp   = defineGramParams args
      -- get the parent directory of filename 
      getParent = reverse . dropWhile (/= slash) . reverse
      dirname   = getParent filename
      -- expand the relative paths 
      toAbs path = if isAbs path        
                   then path
                   else dirname ++ path
      --
      isAbs []    = False
      isAbs (a:_) = (a == slash)
      --
      morphf = morphFile gp
      ldir   = lexiconDir gp
  in gp {
       macrosFile  = toAbs (macrosFile gp),
       lexiconFile = toAbs (lexiconFile gp),
       tsFile      = toAbs (tsFile gp),
       lexiconDir  = if null ldir then "" else toAbs ldir,
       morphFile   = if null morphf then "" else toAbs morphf
     }
\end{code}

\begin{code}
defineGramParams :: [(Token,String)] -> GramParams

defineGramParams [] = GrmPrms {
  macrosFile  = "",
  lexiconFile = "",
  lexiconDir  = "",
  tsFile      = "",
  morphFile   = "",
  rootCatsParam = [],
  grammarType   = GeniHand
}

defineGramParams ((f,v):s) =
  case f of MacrosTok     -> next {macrosFile  = v}
            LexiconTok    -> next {lexiconFile = v} 
            LexiconDirTok -> next {lexiconDir  = v}
            TestSuiteTok  -> next {tsFile = v}
            MorphInfoTok  -> next {morphFile   = v}
            RootCategoriesTok -> next {rootCatsParam = words v}
            GrammarType -> next {grammarType = t} 
                           where t = case (read v) of 
                                       GeniHandTok -> GeniHand 
                                       TAGMLTok    -> TAGML 
                                       CGManifestoTok -> CGManifesto
                                       _           -> error (show v ++ e) 
                                 e = " is not a grammar type"
            p -> error ("Unknown index file parameter: " ++ show p)
  where next = defineGramParams s
\end{code}
