#!/usr/bin/env runhaskell

-- Summarise the output of several HUnit runs
-- Options
--   --invert-pf to treat passes as fails and vice/versa
--   --invert-ef to treat fails as errors and vice/versa
--   --invert-pe to treat passes as errors and vice/versa
-- Public domain: do what you want with this
-- Eric Kow

module Main where

import Control.Monad (liftM, liftM4, when)
import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import System.IO
import Test.HUnit.Base (Counts(Counts, failures, tried, errors))
import Test.HUnit.Terminal (terminalAppearance)
import Text.ParserCombinators.Parsec (parse, Parser,
    spaces, char, many, digit, string,
    )

main :: IO ()
main =
 do as <- getArgs
    let inverters = [ ("--invert-pf" , invertPassFailC)
                    , ("--invert-ef" , invertErrFailC)
                    , ("--invert-pe" , invertPassErrC) ]
    parser <- case [ i | (s,i) <- inverters, s `elem` as ] of
                []  -> return (parseHLine id)
                [i] -> return (parseHLine i)
                _   -> fail "Inverter switches are mutually exclusive"
    ls <- lines `liftM` getContents
    putStrLn . showC . (foldr addC zeroC) . (mapMaybe parser) $ ls

hfields :: [String]
hfields = [ "Cases", "Tried", "Errors", "Failures" ]

-- line parser stuff
parseHLine :: (Counts -> Counts) -> String -> Maybe Counts
parseHLine inv l =
 liftM inv $ either (const Nothing) Just
           $ parse hunitResult ""
           $ terminalAppearance l

hunitResult :: Parser Counts
hunitResult =
 sillyLiftM4 Counts $ map field hfields

field :: String -> Parser Int
field s =
 do string s ; char ':' ; spaces
    r <- read `liftM` many digit
    spaces
    return r

-- hunit stuff
zeroC :: Counts
zeroC = Counts 0 0 0 0

addC :: Counts -> Counts -> Counts
addC (Counts a1 a2 a3 a4) (Counts b1 b2 b3 b4) =
 Counts (a1+b1) (a2+b2) (a3+b3) (a4+b4)

showC :: Counts -> String
showC (Counts c t e f) =
 unwords2 $ zipWith (\f c -> f ++ ":" ++ show c) hfields [c,t,e,f]

invertPassFailC, invertErrFailC, invertPassErrC  :: Counts -> Counts
invertPassFailC c = c { failures = tried c - (failures c) - (errors c) }
invertPassErrC c  = c { errors   = tried c - (failures c) - (errors c) }
invertErrFailC c  = c { failures = errors c, errors = failures c }

-- general stuff
sillyLiftM4 :: Monad m => (x -> x -> x -> x -> r) -> [m x] -> m r
sillyLiftM4 f [a,b,c,d] = liftM4 f a b c d
sillyLiftM4 _ _ = error "sillyLift4 needs exactly 4 args"

unwords2 :: [String] -> String
unwords2 = concat . (intersperse "  ")
