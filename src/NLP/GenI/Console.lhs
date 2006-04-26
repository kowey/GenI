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
module NLP.GenI.Console(consoleGeni) where
\end{code}

\ignore{
\begin{code}
import Data.List(find)
import Control.Monad(when)
import Data.IORef(readIORef, modifyIORef)

import NLP.GenI.General(ePutStrLn, withTimeout, exitTimeout)
import NLP.GenI.Geni
import NLP.GenI.Configuration
  ( Params, isGraphical, outputFile, statsFile, metricsParam, timeoutSecs
  , builderType
  , BuilderType(..))
import qualified NLP.GenI.Builder as B
import NLP.GenI.CkyEarley.CkyBuilder
import NLP.GenI.Simple.SimpleBuilder
import Statistics ( showFinalStats, Statistics )
\end{code}
}

We support exactly one kind of batch processing: Batch testing of
optimisations, that is, we assume that you are working with exactly one test
case.

In the past we used to process entire test suites, but now we can only 
handle one test case at a time.  If you want do process the whole 
test suite, you'll have to write a shell script.

\begin{code}
consoleGeni :: ProgStateRef -> IO()
consoleGeni pstRef = do
  pst <- readIORef pstRef
  when (isGraphical $ pa pst) $ do
    ePutStrLn "GUI not available"
  --
  loadGrammar pstRef
  ePutStrLn "======================================================"
  --
  case timeoutSecs $ pa pst of
    Nothing -> runTestCase pstRef
    Just t  -> withTimeout t (timeoutErr t) $ runTestCase pstRef
  where
   timeoutErr t = do ePutStrLn $ "GenI timed out after " ++ (show t) ++ "s"
                     exitTimeout


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
     let config = pa pst
     (sentences, stats) <- case builderType config of
                            NullBuilder   -> helper B.nullBuilder
                            SimpleBuilder -> helper simpleBuilder_2p
                            SimpleOnePhaseBuilder -> helper simpleBuilder_1p
                            CkyBuilder    -> helper ckyBuilder
                            EarleyBuilder -> helper earleyBuilder
     -- if no output file is set, write to stdout
     let oPutStrLn = if (null pstOutfile) then putStrLn 
                     else writeFile pstOutfile 
     oPutStrLn (unlines sentences)
     -- print out statistical data (if available)
     let sFile      = statsFile config
         soPutStrLn = if (null sFile) then putStrLn else writeFile sFile
     when (not $ null $ metricsParam config) $
       do soPutStrLn $ "begin stats\n" ++ showFinalStats stats ++ "end"
  where
    helper :: B.Builder st it Params -> IO ([String], Statistics)
    helper builder =
      do (sentences, stats, _) <- runGeni pstRef builder
         return (sentences, stats)
\end{code}
