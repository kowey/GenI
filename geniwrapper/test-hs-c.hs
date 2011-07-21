module Main where

import Control.Applicative

import MinimalGenI
import System.Environment ( getArgs )

import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.Ptr
import Foreign.StablePtr

main = do
  args <- getArgs
  (mac, lex, sem) <- case args of
                  [x1,x2,x3] -> return (x1, x2, x3)
                  _ -> fail "Usage: test macro-file lex-file sem-file"
  testSem <- readFile sem
  cm <- newCString mac
  cl <- newCString lex
  pst    <- cGeniInit cm cl
  result <- cGeniRealize pst <$> newCString testSem
  putStrLn =<< peekCString =<< result
