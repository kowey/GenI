\chapter{Configuration}

This module handles configuration parameters such as the input files and
the optimisations that Geni should handle.

\textbf{TODO}:
\begin{enumerate}
\item change name functions in of Params to xFile
\end{enumerate}

\begin{code}
module Configuration(
   Params, GrammarType(..),
   treatArgs, 
   macrosFile, lexiconFile, grammarType,
   tsFile, graphical, 
   optimisations,
   polarised, polsig, chartsharing, extrapol,
   predicting, semfiltered, orderedadj, footconstr,
   isBatch, batchRepeat, 
   defaultParams, emptyParams, getConf, optBatch,

   -- re-export
   Token(Batch)
)

where
\end{code}

\ignore{
Some basic haskell library stuff to import:

\begin{code}
import Data.List (words, intersperse)
import Data.FiniteMap 
\end{code}

We also import some stuff from the rest of the generator.

\begin{code}
import Lex2 (lexer)
import Cparser (cParser)
import PolParser (polParser)
import ParserLib(Token(..))
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

\begin{code}
data GrammarType = GeniHand | TAGML 
     deriving (Show, Eq)
\end{code}

The Params data structure holds the specification for how Geni should be
run, its input files, etc.  This is the stuff that would normally be
found in the configuration file.

\begin{code}
data Params = Prms{
           macrosFile     :: String,
           lexiconFile    :: String,
           grammarType    :: GrammarType,
           tsFile         :: String,
           graphical      :: Bool,
           optimisations  :: [Token],
           extrapol       :: FiniteMap String Int,
           batchRepeat    :: Integer
         } deriving (Show)

polarised    :: Params -> Bool
polsig       :: Params -> Bool
predicting   :: Params -> Bool
semfiltered  :: Params -> Bool
chartsharing :: Params -> Bool
orderedadj   :: Params -> Bool
footconstr   :: Params -> Bool
isBatch      :: Params -> Bool

polarised    p = Polarised    `elem` (optimisations p)
polsig       p = PolSig       `elem` (optimisations p)
predicting   p = Predicting   `elem` (optimisations p)  
semfiltered  p = SemFiltered  `elem` (optimisations p)
chartsharing p = ChartSharing `elem` (optimisations p)
orderedadj   p = OrderedAdj   `elem` (optimisations p)
footconstr   p = FootConstraint `elem` (optimisations p)
isBatch      p = Batch        `elem` (optimisations p)
\end{code}

\paragraph{defaultParams} returns the default parameters configuration

\begin{code}
emptyParams :: Params
emptyParams = Prms {
  macrosFile     = "",
  lexiconFile    = "",
  tsFile         = "",
  grammarType    = GeniHand,
  graphical      = False,
  optimisations  = [],
  extrapol       = emptyFM,
  batchRepeat    = 1
}

defaultParams :: Params
defaultParams = emptyParams {
   macrosFile     = "examples/ej/mac",
   lexiconFile    = "examples/ej/lex",
   tsFile         = "examples/ej/ej1",
   graphical      = True,
   grammarType    = GeniHand
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
                          return (defineParams p (cParser (lexer fconf)))
            createConf = do writeFile ".genirc" (defaultParamsStr p)
                            putStr "File .genirc does not exists.\n"
                            putStr "Writing default configuration file.\n"
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
  params ++ defineParams (last params) args 
  where args = (cParser . lexer . unwords) s
\end{code}


\paragraph{defaultParamsStr} given
  - a Params structure with the default values
returns a string that is used to generate the .genirc default
configuration file
\begin{code}
defaultParamsStr :: Params -> String
defaultParamsStr p = 
  let g  = macrosFile p
      l  = lexiconFile p
      ts = tsFile p
      op = optimisations p
      gr = if (graphical p)  then "True" else "False"
  in "\nMacros   = " ++ g  ++ 
     "\nLexicon  = " ++ l  ++ 
     "\nTSemantics = " ++ ts ++ 
     "\n" ++
     "\n% True or False" ++
     "\nGraphical  = " ++ gr ++ 
     "\n" ++
     "\n% Optimisations should be a comma delimited list containing any " ++
     "\n% number of the following items:" ++
     "\n%  Polarised, PolSig, ChartSharing," ++
     "\n%  SemFiltered, OrderedAdj, FootConstraint" ++
     "\nOptimisations = " ++ 
     "\n" ++ (concat $ intersperse "," $ map show op) ++ 
     "\n% ExtraPolarities should be a list of polarities as in the macro " ++
     "\n% file, but no predictors." ++
     "\nExtraPolarities =" 
\end{code}

% --------------------------------------------------------------------  
\section{Intepreting commands}
% --------------------------------------------------------------------  

The configuration file is intpreted as a list of lists of tokens.  We
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
defineParams' p ((f,v):s) =
  case f of Macros  ->  defineParams' p{macrosFile     = v} s
            Lexicon ->  defineParams' p{lexiconFile    = v} s
            GrammarType -> defineParams' p{grammarType = t} s
                           where t = case (read v) of 
                                       GeniHandTok -> GeniHand 
                                       TAGMLTok    -> TAGML 
                                       _           -> error (show v ++ e) 
                                 e = " is not a grammar type"
            TSemantics -> defineParams' p{tsFile  = v} s
            Graphical  -> defineParams' p{graphical  = (v == "True")} s
            Optimisations   -> defineParams' p{optimisations = readOpt } s
                               where readOpt = map read $ words v
            ExtraPolarities -> defineParams' p{extrapol = (polParser . lexer) v} s
            Repeat -> defineParams' p{batchRepeat = read v} s 
            p -> error ("Unknown configuration parameter" ++ show p)
\end{code}

\paragraph{optBatch} represents all the possible combinations of
optimisations.

\begin{code}
optBatch :: [[Token]] 
optBatch = 
  let polBatch = [ [] , [Polarised] , [Polarised, ChartSharing ] ]
      filBatch = polBatch ++ map (SemFiltered:) polBatch
      adjBatch = filBatch ++ map (OrderedAdj:) filBatch
  in map (FootConstraint:) adjBatch
\end{code}

