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
import Data.List(find,intersperse)
import Control.Monad(when)
import Data.IORef(readIORef, modifyIORef)

import Btypes(SemInput)
import General(ePutStrLn) 
import Geni
import Builder(avgGstats, numcompar, szchart, geniter)
import Configuration(Params, isGraphical, isBatch, outputFile,
                     optimisations, batchRepeat, optBatch) 
\end{code}
}

We support exactly one kind of batch processing: Batch testing of
optimisations, that is, we assume that you are working with exactly one test
case.  We call \fnref{runBatch} to succesively test all the possible
optimisiations.  

In the past we used to process entire test suites, but now we can only 
handle one test case at a time.  If you want do process the whole 
test suite, you'll have to write a shell script.

\begin{code}
consoleGenerate :: ProgStateRef -> IO()
consoleGenerate pstRef = do 
  pst <- readIORef pstRef
  let config = pa pst
  when (isGraphical $ pa pst) $ do
    ePutStrLn "GUI not available for batch processing"
  --
  loadGrammar pstRef
  ePutStrLn "======================================================"
  --
  let batchTestOpts = isBatch config
  if batchTestOpts 
     then runBatch pstRef 
     else runTestSuite pstRef 
\end{code}

\section{Batch testing of optimisations}
\label{fn:runBatch}
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
     ePutStrLn ""
     ePutStrLn $ showOptResults resSet
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
  ePutStrLn $ "------------" 
  ePutStrLn $ "Optimisations: " ++ optStr1 ++ optStr2 
  ePutStrLn $ "Automaton paths explored: " ++ (grAutPaths res)
  ePutStrLn $ "\nRealisations: " 
  ePutStrLn $ showRealisations sentences 
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

\paragraph{runTestSuite} runs a case in the test suite.  If the user does not
specify any test cases, we run the first one.  If the user specifies a non-existing 
test case we raise an error.

\begin{code}
runTestSuite :: ProgStateRef -> IO () 
runTestSuite pstRef = 
  do pst <- readIORef pstRef 
     let pstCase    = tcase pst
         pstSuite   = tsuite pst
         selection  = find (\x -> fst x == pstCase) pstSuite
         pstOutfile = outputFile $ pa pst
     sem <- if null pstCase
               then if null pstSuite 
                       then fail "Test suite is empty."
                       else return (snd $ head pstSuite)
               else case selection of 
                      Nothing -> fail ("No such test case: " ++ pstCase)
                      Just s  -> return $ snd s
     res <- runTestCase pstRef sem
     -- if no output file is set, write to stdout
     let oPutStrLn = if (null pstOutfile) then putStrLn 
                     else writeFile pstOutfile 
     oPutStrLn $ unlines $ grSentences res
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


