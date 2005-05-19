{-
  This module's sole job is to decide between the text/graphical 
  interface.
-}

{- TODO Define what is and what is not exported from the modules.  In particular
        in BTypes take care to export the inspection function but not the types.  
        Re-write functions in Main as needed.-}

{- TODO Change input in Lexicon and Grammar to allow more than one anchor.-}

{- TODO Keys used in Tags are specially bad for Pn, perhaps they can be improved.-}

module Main (main)
 
where

import Data.IORef(readIORef)
import Geni(initGeni, pa, batchPa)
import Gui(guiGenerate)
import Console(consoleGenerate)

import Configuration(isGraphical, isBatch)

main :: IO ()

main = do       
  pst <- initGeni
  mst <- readIORef pst
  let headPa   = pa mst
  let notBatch  = (  ((length $ batchPa mst) == 1) 
                  && (not $ isBatch headPa))
      graphical = isGraphical headPa
  if (graphical && notBatch) 
     then guiGenerate pst
     else consoleGenerate pst
