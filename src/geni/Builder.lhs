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

\chapter{Builder}
\label{cha:Builder}

This module provides an interface to the various back-ends that GenI 
provides.  

Part of the problem is this conflict between wanting to make black
boxes out of stuff and wanting to be able to inspect it with the
graphical interface.  So this stuff isn't really worked out very
well yet.

\begin{code}
module Builder 
where
\end{code}

\ignore{
\begin{code}
import Control.Monad.State
import Data.Maybe ( mapMaybe, isNothing )

import Automaton (NFA)
import Configuration ( Params(metricsParam) )
import General (geniBug)
import Btypes    (ILexEntry, SemInput, Flist)
import Statistics (Statistics, incrIntMetric,
                   Metric(IntMetric), updateMetrics,
                   mergeMetrics, addIntMetrics,
                   queryMetrics, queryIntMetric,
                   addMetric, emptyStats,
                   )
import Tags      (TagElem)
\end{code}
}

All backends provide the same essential functionality:
\begin{description}
\item [run]       calls init and stepAll and potentially wraps it with some
                  other functionality.  
\item [init]      initialise the machine from the semantics and lexical selection 
\item [step]      run a realisation step
\item [stepAll]   run all realisations steps until completion
\item [finished]  determine if realisation is finished
\item [stats]     extract various statistics from it
\item [setStats]  set the statistical information 
\item [unpack]    unpack chart results into a list of sentences
\end{description}

\begin{code}
data Builder st it pa = Builder
  { init     :: Input -> pa -> (st, Statistics)
  --
  , step     :: BuilderState st ()
  , stepAll  :: BuilderState st ()
  , run      :: Input -> pa -> (st, Statistics)
  --
  , finished :: st -> Bool
  , unpack   :: st -> [UninflectedSentence] }
\end{code}

\begin{code}
data BuilderGui = BuilderGui
  { generateGui :: IO ()
  , debugGui    :: IO () }
\end{code}

To simplify interaction with the backend, we provide a single data
structure which represents all the inputs a backend could take.

\begin{code}
data Input = 
  Input { inSemInput :: SemInput
        , inLex      :: [ILexEntry]  -- debugger
        , inCands    :: [TagElem]
        }
\end{code}

\section{Using builders}

\fnlabel{defaultStepAll} provides a default implementation for
Builder's \fnlabel{stepAll} function.

\begin{code}
defaultStepAll :: Builder st it pa -> BuilderState st ()
defaultStepAll b = 
 do s <- get
    unless (finished b s) $ 
      do step b
         defaultStepAll b
\end{code}

\section{Uninflected words and sentences}

Each word of an uninflected sentence consists of a lemma and some
feature structures.

\paragraph 
A SentenceAut represents a set of sentences in the form of an automaton.
The labels of the automaton are the words of the sentence.  But note! 
``word'' in the sentence is in fact a tuple (lemma, inflectional feature
structures).  Normally, the states are defined as integers, with the
only requirement being that each one, naturally enough, is unique.

\begin{code}
type UninflectedWord        = (String, Flist)
type UninflectedSentence    = [ UninflectedWord ] 
type UninflectedDisjunction = ([String], Flist)
type SentenceAut            = NFA Int UninflectedWord 
\end{code}

\section{BuilderState}

To cleanly seperate the tracking of statistics from the core functionality of a
builder, we use a State transformer to thread a Statistics state monad inside of
our main monad.

\begin{code}
type BuilderState s a = StateT s (State Statistics) a
\end{code}

\section{Statistics}

\begin{code}
addCounters :: Statistics -> Statistics -> Statistics
addCounters = mergeMetrics addIntMetrics

modifyStats :: (Metric -> Metric) -> BuilderState st ()
modifyStats fn = lift $ modify $ updateMetrics fn

incrCounter :: String -> Int -> BuilderState st ()
incrCounter key n = modifyStats (incrIntMetric key n)

queryCounter :: String -> Statistics -> Int
queryCounter key s =
  case queryMetrics (queryIntMetric key) s of
  []  -> geniBug $ "No instances of the metric: " ++ key
  [c] -> c
  _   -> geniBug $ "More than one instance of the metric: " ++ key
\end{code}

\subsection{Command line configuration}

\begin{code}
initStats :: Params -> Statistics
initStats pa =
 let identifyMs :: [String] -> ([Metric],[String])
     identifyMs ["default"] = identifyMs defaultMetricNames
     identifyMs ms =
      (mapMaybe namedMetric ms, [ m | m <- ms, (isNothing.namedMetric) m ])
     --
     (metrics, unknown) = identifyMs $ metricsParam pa
 in if null unknown
    then execState (mapM addMetric metrics) emptyStats
    else error $ "Unknown metrics: " ++ (show unknown)

namedMetric :: String -> Maybe Metric
namedMetric n | n `elem` knownIntMetricNames = Just $ IntMetric n 0
namedMetric _ = Nothing

-- Note that the strings here are command-line strings, not metric names!
defaultMetricNames :: [ String ]
defaultMetricNames = [ num_iterations, chart_size, num_comparisons ]

-- Unlike the defaultMetrics above, the names below are metric key names.
knownIntMetricNames :: [ String ]
knownIntMetricNames = defaultMetricNames
\end{code}

\subsection{Common counters}

These numbers allow us to keep track of how efficient our generator is
and where we are in the process (how many steps we've taken, etc)

\begin{code}
num_iterations, chart_size, num_comparisons :: String

num_iterations  = "iterations"
chart_size      = "chart_size"
num_comparisons = "comparisons"
\end{code}
