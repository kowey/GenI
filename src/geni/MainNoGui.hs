{- TODO Prepare a report from the slides of the talk -}

{- TODO To eliminate redundant generations enforce that substitution
        tree should have an empty list of substitution nodes when applying 
        Substitution -}

{- TODO Parser of Lexicon gives error when the input has empty parameters.  This
        should probably be permited. Similar for Grammar-}

{- TODO Define what is and what is not exported from the modules.  In particular
        in BTypes take care to export the inspection function but not the types.  
        Re-write functions in Main as needed.-}

{- TODO Change input in Lexicon and Grammar to allow more than one anchor.-}

{- TODO Keys used in Tags are specially bad for Pn, perhaps they can be improved.-}

module Main (main)
 
where

import Data.List(intersperse, group, sort)
import Monad(mapM, foldM, when)

import IOExts(readIORef, modifyIORef)
import Treeprint
import Geni
import Configuration(graphical, grammar, lexicon, tsFile, 
                     emptyParams, optimisations, isBatch,
                     optBatch, Token(..))
import Mstate (generate)
import Polarity

{-----------------------------------------------------------------------}
{- Main                                                                -}
{-----------------------------------------------------------------------}

main :: IO ()

main = do       
  pst <- initGeni
  mst <- readIORef pst
  let headPa   = pa mst
  consoleGenerate pst

consoleGenerate pst = do 
  let nogui =  "Graphical interface not available for "
             ++ "batch processing"
  mst <- readIORef pst
  when (graphical $ pa mst) (putStrLn nogui)
  foldM (consoleGenerate' pst) emptyParams (batchPa mst)
  return ()
  
-- for each grammar/lexicon/target semantics 
consoleGenerate' pst lastPa newPa = do 
  modifyIORef pst (\x -> x{pa = newPa})
  putStrLn "======================================================"
  -- only load files if neccesary
  let lastGrammar   = grammar lastPa
      lastLexicon   = lexicon lastPa
      lastTargetSem = tsFile lastPa
  when (lastGrammar /= grammar newPa)  $ loadGrammar pst
  when (lastLexicon /= lexicon newPa)  $ loadLexicon pst
  when (lastTargetSem /= tsFile newPa) $ loadTargetSem pst
  -- determine if we have to run a batch of optimisations 
  let batch = map (\o -> newPa { optimisations = o }) optBatch  
  if (Batch `elem` optimisations newPa) 
     then do resSet <- mapM (consoleGenerate'' pst) batch
             putStrLn ""
             putStrLn $ showOptResults resSet
             return ()
     else do res <- verboseGeni pst 
             putStrLn $ show res
  return newPa

-- for each set of optimisations...
consoleGenerate'' pst newPa = do 
  modifyIORef pst (\x -> x{pa = newPa})
  res <- verboseGeni pst 
  let derived = grDerived res
      optPair = grOptStr res
      optStr1 = fst optPair
      optStr2 = if (optStr1 /= "none ") then ("(" ++ snd optPair ++ ")") else ""
  putStrLn $ "------------" 
  putStrLn $ "Optimisations: " ++ optStr1 ++ optStr2 
  putStrLn $ "Automaton paths explored: " ++ (grAutPaths res)
  putStrLn $ "\nRealisations: " 
  putStrLn $ showRealisations derived 
  return res

