{- This is used for compiling without WxHaskell -}

module Main (main)
 
where

import IOExts(readIORef, modifyIORef)
import Geni
import Console(consoleGenerate)
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

