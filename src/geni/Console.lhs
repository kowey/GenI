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

\chapter{Console}

This module handles the console user interface, batch processing, and test
suites.  Test suites could be seen as a type of batch processing, but they
produce fancy HTML reports, oh la la!

\begin{code}
module Console(consoleGenerate) where
\end{code}

\ignore{
\begin{code}
import Data.List(intersperse,sort,partition)
import Monad(mapM, foldM, when)
import IOExts(readIORef, modifyIORef)

import Bfuncs(SemInput,showSem)
import Geni
import Mstate(avgGstats, numcompar, szchart, geniter)
import Configuration(Params, isGraphical, isTestSuite,
                     isBatch,
                     grammarFile, tsFile, 
                     emptyParams, optimisations, batchRepeat,
                     optBatch) 
\end{code}
}

\section{Outer layer}

There are three kinds of batch processing:

\begin{itemize}
\item Vanilla batch processing - multiple entries in .genirc
\item Batch testing of optimisations (in each entry)
\item Test suite with multiple semantic items (in each entry)
\end{itemize}

The outer layer runs the middle layer over each vanilla batch processing item.

\begin{code}
consoleGenerate :: PState -> IO()
consoleGenerate pst = do 
  let nogui =  "Graphical interface not available for "
             ++ "batch processing"
  mst <- readIORef pst
  when (isGraphical $ pa mst) (putStrLn nogui)
  foldM (consoleGenerate' pst) emptyParams (batchPa mst)
  return ()
\end{code}

The middle layer operates on a single entry: It loads the grammar file
and the target semantics (if they are different from the last time),
runs the generator, and returns the updated parameters 
(so that in the future we can determine if we have to reload stuff).

We run the generator in the following manner:

\begin{enumerate}
\item If we have a test-suite instead of a target semantics, then we run the
generator for each entry in the test suite and print some fancy HTML
tables in the tmp directory.  
\item If there is batch processing on the
optimisations, we run the inner layer over each optimisation and then
pretty-print a summary table.
\item Finally, if there is neither test-suite nor batch processing, we
simply run the generator and print out the results.
\end{enumerate}

Note: if there is a test-suite and batch if batch processing is
set, we simply ignore the batch processing.

\begin{code}
consoleGenerate' :: PState -> Params -> Params -> IO Params
consoleGenerate' pst lastPa newPa = do 
  modifyIORef pst (\x -> x{pa = newPa})
  putStrLn "======================================================"
  -- only load files if neccesary
  let lastGrammar    = grammarFile lastPa
      lastTargetSem  = tsFile lastPa
  when (lastGrammar /= grammarFile newPa)  $ loadGrammar pst
  when (lastTargetSem /= tsFile newPa)     $ loadTargetSem pst
  -- determine how we should run the generator
  let runVanilla = do res <- customGeni pst runGeni
                      putStrLn $ show res
  --
  case () of _ | isTestSuite newPa -> runTestSuite pst 
               | isBatch newPa     -> runBatch pst 
               | otherwise         -> runVanilla  
  return newPa
\end{code}

\section{Batch testing of optimisations}

\paragraph{runBatch} runs a batch processing suite and prints the
results.  We assume that the grammar and target semantics are already
loaded into the monadic state.

\begin{code}
runBatch :: PState -> IO ()
runBatch pst = 
  do mst <- readIORef pst 
     let curPa = pa mst
         batch = map (\o -> curPa { optimisations = o }) optBatch  
     resSet <- mapM (runBatchSample pst) batch
     putStrLn ""
     putStrLn $ showOptResults resSet
     return ()
\end{code}

\paragraph{runSample} is used to run a single sample for batch
processing of optimisations for as many iterations as requested. The
inner layer runs the generator and prints a reduced version of the
summary.  The purpose is to reduce the amount of redundant information
being displayed to the user; and to summarise everything in a fancy
table.

\begin{code}
runBatchSample :: PState -> Params -> IO GeniResults
runBatchSample pst newPa = do 
  modifyIORef pst (\x -> x{pa = newPa})
  let numIter = batchRepeat newPa
  resSet <- mapM (\_ -> customGeni pst runGeni) [1..numIter]
  --
  let avgStats  = avgGstats $ map grStats resSet
      res       = (head resSet) { grStats = avgStats } 
      sentences = grSentences res
      optPair   = grOptStr res
      optStr1   = fst optPair
      optStr2   = if (optStr1 /= "none ") then ("(" ++ snd optPair ++ ")") else ""
  --
  putStrLn $ "------------" 
  putStrLn $ "Optimisations: " ++ optStr1 ++ optStr2 
  putStrLn $ "Automaton paths explored: " ++ (grAutPaths res)
  putStrLn $ "\nRealisations: " 
  putStrLn $ showRealisations sentences 
  return res
\end{code}

\paragraph{showOptResults} displays a list of performance results in a
single table.  The intention is for each item in the list to be the 
result of a different optimisation on the same grammar/semantics

\begin{code}
showOptResults :: [GeniResults] -> String
showOptResults grs = 
  let headOpt = "      optimisations"
      headNumRes = "rslts"
      headAgenda = "agnd sz"
      headChart  = "chrt sz"
      headComparisons = "compared"
      headTime = "time ms  "
      header   = [ headOpt, headNumRes, headAgenda, headChart, headComparisons, headTime ] 
      showIt l = concat $ intersperse " | " $ l
      showLine = concat $ intersperse "-+-" $ map linestr header
      resStr r = [ pad (fst  $ grOptStr r) headOpt,
                   pad (show $ length $ grDerived r) headNumRes,
                   pad (show $ geniter s) headAgenda,
                   pad (show $ szchart s) headChart,
                   pad (show $ numcompar s) headComparisons, 
                   pad (grTimeStr r) headTime ]
                 where s = grStats r
      -- a list of "-" with the same length as l 
      linestr str2 = map (const '-') str2
      -- pad str to be as long as str2
      pad str str2 = if (diff > 0) then padding ++ str else str
                     where padding = map (const ' ') [1..diff]
                           diff = (length str2) - (length str)   
      --
      headerStr = showIt header ++ "\n" ++ showLine ++ "\n" 
      bodyStr   = concat $ intersperse "\n" $ map (showIt.resStr) grs
  in headerStr ++ bodyStr
\end{code}

\section{Test suites}

\paragraph{runTestSuite} runs a test suite and summarises the results

\begin{code}
runTestSuite :: PState -> IO () 
runTestSuite pst = 
  do mst <- readIORef pst 
     let (slist, xlist) = (unzip . tsuite) mst
     rlist <- mapM (runTestCase pst) slist 
     let summaries = zipWith3 showTestCase slist xlist rlist
     mapM putStrLn summaries 
     return ()
\end{code}

\paragraph{runTestCase} runs a single case in a test suite and returns
the sentences generated.

\begin{code}
runTestCase :: PState -> SemInput -> IO [String]
runTestCase pst sem = 
  do modifyIORef pst (\x -> x{ts = sem})
     res <- customGeni pst runGeni
     let sentences = grSentences res
     return sentences
\end{code}

\paragraph{showTestCase} returns a pretty-printed string summarising
the results of running a test case.  Note that for 

\begin{code}
showTestCase :: SemInput -> [String] -> [String] -> String
showTestCase (sem,_) expected results = 
  let expected2     = sort expected
      results2      = sort results
      --
      (pass,overgen) = partition expfn results2
                       where expfn x = x `elem` expected2
      fail           = filter (not.resfn) expected2 
                       where resfn x = x `elem` results2 
      --
  in ""
     ++ "\n================================================================="
     ++ "\n" ++ showSem sem 
     ++ "\n================================================================="
     ++ "\n" 
     ++ (if null fail 
        then "" 
        else "\nfail" 
             ++ "\n----"
             ++ "\n" ++ showRealisations fail
             ++ "\n")
     ++ (if null pass 
        then ""
        else "\npass"
             ++ "\n----"
             ++ "\n" ++ showRealisations pass 
             ++ "\n")
     ++ (if null overgen
        then ""
        else "\novergeneration"
             ++ "\n--------------"
             ++ "\n" ++ showRealisations overgen)
\end{code}


