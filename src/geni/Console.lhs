\chapter{Console}

This module handles the console user interface, batch processing, and test
suites.  Test suites could be seen as a type of batch processing, but they
produce fancy HTML reports, oh la la!

\begin{code}
module Console(consoleGenerate) where
\end{code}

\ignore{
\begin{code}
import Monad(mapM, foldM, when)

import IOExts(readIORef, modifyIORef)
import Geni
import Mstate(avgGstats)
import Configuration(Params, isGraphical, 
                     grammarFile, tsFile, 
                     emptyParams, optimisations, batchRepeat,
                     optBatch, Token(..))
\end{code}
}

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
and the target semantics (if they are different from the last time).  

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
  when (lastTargetSem /= tsFile newPa) $ loadTargetSem pst
  -- determine if we have to run a batch of optimisations 
  let batch = map (\o -> newPa { optimisations = o }) optBatch  
  if (Batch `elem` optimisations newPa) 
     then do resSet <- mapM (consoleGenerate'' pst) batch
             putStrLn ""
             putStrLn $ showOptResults resSet
             return ()
     else do res <- customGeni pst runGeni
             putStrLn $ show res
  return newPa
\end{code}

The inner layer runs the generator and prints a reduced version of 
the summary.  This is only used when the optimisations = Batch; the
purpose is to reduce the amount of redundant information being 
displayed to the user; and to summarise everything in a fancy table.

\begin{code}
consoleGenerate'' :: PState -> Params -> IO GeniResults
consoleGenerate'' pst newPa = do 
  modifyIORef pst (\x -> x{pa = newPa})
  let numIter = batchRepeat newPa
  resSet <- mapM (\_ -> customGeni pst runGeni) [1..numIter]
  --
  let avgStats = avgGstats $ map grStats resSet
      res      = (head resSet) { grStats = avgStats } 
      derived  = grDerived res
      optPair  = grOptStr res
      optStr1  = fst optPair
      optStr2  = if (optStr1 /= "none ") then ("(" ++ snd optPair ++ ")") else ""
  putStrLn $ "------------" 
  putStrLn $ "Optimisations: " ++ optStr1 ++ optStr2 
  putStrLn $ "Automaton paths explored: " ++ (grAutPaths res)
  putStrLn $ "\nRealisations: " 
  putStrLn $ showRealisations derived 
  return res
\end{code}
