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
suites.  

\begin{code}
module Console(consoleGenerate) where
\end{code}

\ignore{
\begin{code}
import Data.List(intersperse,sort,partition)
import Control.Monad(foldM, when)
import Data.IORef(readIORef, modifyIORef)

import Bfuncs(SemInput,showSem)
import General(fst3,snd3,thd3)
import Geni
import Mstate(avgGstats, numcompar, szchart, geniter)
import Configuration(Params, isGraphical, isBatch,
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
consoleGenerate :: ProgStateRef -> IO()
consoleGenerate pstRef = do 
  let nogui =  "Graphical interface not available for "
             ++ "batch processing"
  pst <- readIORef pstRef
  when (isGraphical $ pa pst) (putStrLn nogui)
  foldM (consoleGenerate' pstRef) emptyParams (batchPa pst)
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
consoleGenerate' :: ProgStateRef -> Params -> Params -> IO Params
consoleGenerate' pstRef lastPa newPa = do 
  modifyIORef pstRef (\x -> x{pa = newPa})
  putStrLn "======================================================"
  -- only load files if neccesary
  let lastGrammar    = grammarFile lastPa
      lastTargetSem  = tsFile lastPa
  when (lastGrammar /= grammarFile newPa)  $ loadGrammar pstRef
  when (lastTargetSem /= tsFile newPa)     $ loadTestSuite pstRef
  -- determine how we should run the generator
  let runVanilla = do res <- runGeni pstRef doGeneration
                      putStrLn $ show res
  --
  case () of _ | isBatch newPa     -> runBatch pstRef 
               | otherwise         -> runTestSuite pstRef 
  return newPa
\end{code}

\section{Batch testing of optimisations}

\paragraph{runBatch} runs a batch processing suite and prints the
results.  We assume that the grammar and target semantics are already
loaded into the monadic state.

\begin{code}
runBatch :: ProgStateRef -> IO ()
runBatch pstRef = 
  do pst <- readIORef pstRef 
     let curPa = pa pst
         batch = map withopt $ optBatch (optimisations curPa)
                 where withopt o = curPa { optimisations = o } 
     resSet <- mapM (runBatchSample pstRef) batch
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
runBatchSample :: ProgStateRef -> Params -> IO GeniResults
runBatchSample pstRef newPa = do 
  modifyIORef pstRef (\x -> x{pa = newPa})
  let numIter = batchRepeat newPa
  resSet <- mapM (\_ -> runGeni pstRef doGeneration) [1..numIter]
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
  let header   = [ "      optimisations" 
                 , "rslts"
                 , "agnd sz"
                 , "chrt sz"
                 , "compared"
                 , "time ms  " ]
      display r = [ fst  $ grOptStr r ,
                    show $ length $ grDerived r,
                    show $ geniter s,
                    show $ szchart s,
                    show $ numcompar s,
                    grTimeStr r ]
                 where s = grStats r
  in showTable header grs display
\end{code}

\section{Test suites}

\paragraph{runTestSuite} runs a test suite and summarises the results

\begin{code}
runTestSuite :: ProgStateRef -> IO () 
runTestSuite pstRef = 
  do pst <- readIORef pstRef 
     let mstCases  = tcases pst
         mstSuite  = tsuite pst
         matchFn y = [ x | x <- mstSuite, fst3 x ==  y ]
         suite  = if null mstCases 
                  then mstSuite 
                  else concatMap matchFn mstCases
     let (ids, slist, xlist) = unzip3 suite
     rlist <- mapM (runTestCase pstRef) slist 
     let rsList  = map grSentences rlist
         pfoList = zipWith groupTestCaseResults xlist rsList
         details = zipWith3 showTestCase ids slist pfoList 
     -- show a summary
     putStrLn (showTestSuiteResults $ zip3 ids pfoList rlist)
     -- show all the details
     mapM putStrLn details 
     return ()
\end{code}

\paragraph{runTestCase} runs a single case in a test suite and returns
the results.

\begin{code}
runTestCase :: ProgStateRef -> SemInput -> IO GeniResults
runTestCase pstRef sem = 
  do modifyIORef pstRef (\x -> x{ts = sem})
     res <- runGeni pstRef doGeneration
     return res 
\end{code}

\paragraph{groupTestCaseResults} groups the results of a test case into a three
tuple (pass,fail,overgeneration) 

\begin{code}
type TestCaseResults = ([String],[String],[String])
groupTestCaseResults :: [String] -> [String] -> TestCaseResults
groupTestCaseResults expected results = 
  let expected2     = sort expected
      results2      = sort results
      --
      (pass,overgen) = partition expfn results2
                       where expfn x = x `elem` expected2
      fail           = filter (not.resfn) expected2 
                       where resfn x = x `elem` results2 
  in (pass,fail,overgen)
\end{code}

\begin{code}
showTestCase :: String -> SemInput -> TestCaseResults -> String
showTestCase id (sem,_) results = 
  let (pass,fail,overgen) = results
  in ""
     ++ "\n================================================================="
     ++ (if (null id) then "" else "\n" ++ id)
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

\paragraph{showTestSuiteResults} shows a summary of the test suite run, including
for each test case, its name, the number of passes, fails, and overgenerations and
the generation time.

\begin{code}
showTestSuiteResults :: [(String,TestCaseResults,GeniResults)] -> String
showTestSuiteResults items =
  let header = [ "name             "
               , "pass    "
               , "fail    "
               , "overgen "
               , "time ms  " ]
      display :: (String,TestCaseResults,GeniResults) -> [String]
      display (id,pfo,r) = [ id
                           , show $ length (fst3 pfo)
                           , show $ length (snd3 pfo)
                           , show $ length (thd3 pfo)
                           , grTimeStr r ]
  in showTable header items display 
\end{code}

\section{Generic}

\paragraph{showTable} pretty-prints an ASCII table from a list of items.
More precisely, it builds this from 
\begin{enumerate}
\item \fnparam{header} a list of headers, 
\item \fnparam{items}  a list of items and
\item \fnparam{displayfn} which converts the items to list of pretty-printed strings.
\end{enumerate}
Each item corresponds to a row.  The list returned by \fnparam{displayfn} ought
to be the same length as \fnparam{header}, since each item in the list
corresponds to a column.  Note that this function tries to make the table
pretty by padding each column to be same length as the header 
(so to adjust the size of columns, just pad the header with spaces).

\begin{code}
showTable :: [String] -> [a] -> (a -> [String]) -> String
showTable header items displayfn = 
  let showIt l = concat $ intersperse " | " $ l
      showLine = concat $ intersperse "-+-" $ map linestr header
      resStr r = zipWith pad (displayfn r) header
      -- a list of "-" with the same length as l 
      linestr str2 = map (const '-') str2
      -- pad str to be as long as str2
      pad str str2 = if (diff > 0) then padding ++ str else str
                     where padding = map (const ' ') [1..diff]
                           diff = (length str2) - (length str)   
      --
      headerStr = showIt header ++ "\n" ++ showLine ++ "\n" 
      bodyStr   = concat $ intersperse "\n" $ map (showIt.resStr) items 
  in headerStr ++ bodyStr
\end{code}


