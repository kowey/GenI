module Main where

import MinimalGenI
import System.Environment ( getArgs )

main = do
  args <- getArgs
  (mac, lex, lex2, sem) <- case args of
                  [x1,x2,x3,x4] -> return (x1, x2, x3, x4)
                  _ -> fail "Usage: test macro-file lex-file lex-file-2 sem-file"
  lexStr2 <- readFile lex2
  testSem <- readFile sem
  pst <- either (fail . showGenIException) return =<< geniInit mac lex
  putStrLn =<< geniRealize pst Nothing        testSem
  putStrLn =<< geniRealize pst (Just lexStr2) testSem

-- evidence that it was caught in the wrapper
showGenIException e = "Wrapper caught an exception from GenI:\n" ++ show e
