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
  (mac, lex, lex2, sem, rf) <- case args of
                  [x1,x2,x3,x4,x5] -> return (x1, x2, x3, x4, x5)
                  _ -> fail "Usage: test macro-file lex-file lex-file-2 sem-file root-feature"
  testSem <- readFile sem
  lex2Str <- readFile lex2
  cm  <- newCString mac
  cl  <- newCString lex
  cl2 <- newCString lex2Str
  crf <- newCString rf
  pst    <- cGeniInit cm cl
  result  <- cGeniRealize pst <$> newCString testSem <*> pure crf
  putStrLn =<< peekCString =<< result
  result2 <- cGeniRealizeWith pst <$> newCString lex2Str <*> newCString testSem <*> pure crf
  putStrLn =<< peekCString =<< result2
