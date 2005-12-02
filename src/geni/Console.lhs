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
import Data.List(find)
import Control.Monad(when)
import Data.IORef(readIORef, modifyIORef)

import General(ePutStrLn) 
import Geni
import Configuration(Params, isGraphical, outputFile)
import DerivationsBuilder
import SimpleBuilder
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
  when (isGraphical $ pa pst) $ do
    ePutStrLn "GUI not available"
  --
  loadGrammar pstRef
  ePutStrLn "======================================================"
  --
  runTestCase pstRef 
  -- let batchTestOpts = isBatch config
  -- if batchTestOpts then runBatch pstRef else 
\end{code}

%\section{Batch testing of optimisations}
%\label{fn:runBatch}
%\paragraph{runBatch} runs a batch processing suite and prints the
%results.  We assume that the grammar and target semantics are already
%loaded into the monadic state.
%
%\begin{code}
%runBatch :: ProgStateRef -> IO ()
%runBatch pstRef = 
%  do pst <- readIORef pstRef 
%     let curPa = pa pst
%         batch = map withopt $ optBatch (optimisations curPa)
%                 where withopt o = curPa { optimisations = o } 
%     resSet <- mapM (runBatchSample pstRef) batch
%     ePutStrLn ""
%     ePutStrLn $ showOptResults resSet
%     return ()
%\end{code}
%
%\paragraph{runSample} is used to run a single sample for batch
%processing of optimisations for as many iterations as requested. The
%inner layer runs the generator and prints a reduced version of the
%summary.  The purpose is to reduce the amount of redundant information
%being displayed to the user; and to summarise everything in a fancy
%table.
%
%\begin{code}
%runBatchSample :: ProgStateRef -> Params -> IO (GeniResults TagElem)
%runBatchSample pstRef newPa = do 
%  modifyIORef pstRef (\x -> x{pa = newPa})
%  let numIter = batchRepeat newPa
%  resSet <- mapM (\_ -> runGeni pstRef simpleBuilder) [1..numIter]
%  --
%  let avgStats  = avgGstats $ map grStats resSet
%      res       = (head resSet) { grStats = avgStats } 
%      sentences = grSentences res
%      optPair   = grOptStr res
%      optStr1   = fst optPair
%      optStr2   = if (optStr1 /= "none ") then ("(" ++ snd optPair ++ ")") else ""
%  --
%  ePutStrLn $ "------------" 
%  ePutStrLn $ "Optimisations: " ++ optStr1 ++ optStr2 
%  ePutStrLn $ "Automaton paths explored: " ++ (grAutPaths res)
%  ePutStrLn $ "\nRealisations: " 
%  ePutStrLn $ showRealisations sentences 
%  return res
%\end{code}
%
%\paragraph{showOptResults} displays a list of performance results in a
%single table.  The intention is for each item in the list to be the 
%result of a different optimisation on the same grammar/semantics
%
%\begin{code}
%showOptResults :: [GeniResults TagElem] -> String
%showOptResults grs = 
%  let header   = [ "      optimisations" 
%                 , "rslts"
%                 , "agnd sz"
%                 , "chrt sz"
%                 , "compared"
%                 , "time ms  " ]
%      display r = [ fst  $ grOptStr r ,
%                    show $ length $ grDerived r,
%                    show $ geniter s,
%                    show $ szchart s,
%                    show $ numcompar s,
%                    grTimeStr r ]
%                 where s = grStats r
%  in showTable header grs display
%\end{code}
%

\section{Test suites}

\paragraph{runTestCase} runs a case in the test suite.  If the user does not
specify any test cases, we run the first one.  If the user specifies a non-existing 
test case we raise an error.

\begin{code}
runTestCase :: ProgStateRef -> IO () 
runTestCase pstRef = 
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
     --
     modifyIORef pstRef (\x -> x{ts = sem})
     (sentences, _) <- runGeni pstRef simpleBuilder
     -- if no output file is set, write to stdout
     let oPutStrLn = if (null pstOutfile) then putStrLn 
                     else writeFile pstOutfile 
     oPutStrLn (unlines sentences)
     return ()
\end{code}
