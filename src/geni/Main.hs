{-
  This module has three jobs: deciding between the text/graphical 
  interface, providing the text interface, batch processing, if 
  relevant.
-}

{- TODO Prepare a report from the slides of the talk -}

{- TODO Parser of Lexicon gives error when the input has empty parameters.  This
        should probably be permited. Similar for Grammar-}

{- TODO Define what is and what is not exported from the modules.  In particular
        in BTypes take care to export the inspection function but not the types.  
        Re-write functions in Main as needed.-}

{- TODO Change input in Lexicon and Grammar to allow more than one anchor.-}

{- TODO Keys used in Tags are specially bad for Pn, perhaps they can be improved.-}

module Main (main)
 
where

import Monad(mapM, foldM, when)

import IOExts(readIORef, modifyIORef)
import Gui(guiGenerate)
import Geni
import Mstate(avgGstats)
import Configuration(Params, graphical, 
                     grammarFile, tsFile, 
                     emptyParams, optimisations, isBatch, batchRepeat,
                     optBatch, Token(..))

{-----------------------------------------------------------------------}
{- Main                                                                -}
{-----------------------------------------------------------------------}

main :: IO ()

main = do       
  pst <- initGeni
  mst <- readIORef pst
  let headPa   = pa mst
  let notBatch = (length (batchPa mst) == 1) && (not $ isBatch headPa)
      isGraphical = (graphical headPa)
  if (notBatch && isGraphical) 
     then guiGenerate pst
     else consoleGenerate pst

consoleGenerate :: PState -> IO()
consoleGenerate pst = do 
  let nogui =  "Graphical interface not available for "
             ++ "batch processing"
  mst <- readIORef pst
  when (graphical $ pa mst) (putStrLn nogui)
  foldM (consoleGenerate' pst) emptyParams (batchPa mst)
  return ()
  
-- for each macrosFile/lexiconFile/target semantics 
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

-- for each set of optimisations...
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

