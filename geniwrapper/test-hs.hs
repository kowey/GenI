module Main where

import MinimalGenI
import System.Environment ( getArgs )

main = do
  args <- getArgs
  (mac, lex, sem) <- case args of
                  [x1,x2,x3] -> return (x1, x2, x3)
                  _ -> fail "Usage: test macro-file lex-file sem-file"
  lexStr  <- readFile lex
  testSem <- readFile sem
  pst <- either (fail . showGenIException) return =<< geniInit mac
  putStrLn =<< geniRealize pst lexStr testSem

-- evidence that it was caught in the wrapper
showGenIException e = "Wrapper caught an exception from GenI:\n" ++ show e
