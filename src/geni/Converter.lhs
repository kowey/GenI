\chapter{Converter}

This module is meant to act as a standalone program which serves
strictly as a converter between the formats recognised by GenI.  The
current use is to experiment with the idea that the yacc parser for
GeniHand might be much faster than that for the TAGML format, so we
try to see if converting from TAGML to GeniHand is worthwhile.

\begin{code}
module Main (main) where
\end{code}

\ignore{
\begin{code}
import Data.FiniteMap
import Monad(when)
import System (ExitCode(ExitFailure), 
               exitWith, getArgs, getProgName)
import System.IO(getContents)

import Btypes (itreename, iword)
import GrammarXml (parseXmlGrammar, parseXmlLexicon)
import Treeprint (toGeniHand)
\end{code}
}

\begin{code}
main :: IO ()
main = do       
  -- we take exactly one argument: a grammar index file name
  args <- getArgs
  progname <- getProgName
  let usage p = "usage: " ++ p ++ " [--macros|--lexicon] < input > output"
      showusage = do putStrLn (usage progname)
                     exitWith (ExitFailure 1)
  when (length args /= 1) showusage
  let filetype = head args
  case filetype of 
    "--macros"  -> convertMacros 
    "--lexicon" -> convertLexicon 
    _           -> showusage
\end{code}

\begin{code}
convertLexicon :: IO ()
convertLexicon = 
  do lf <- getContents 
     let lex = parseXmlLexicon lf
         showlex l = (iword l) ++ " " ++ (itreename l) ++ "\n"
         outstr    = concatMap showlex lex
     putStr outstr 

convertMacros :: IO ()
convertMacros = 
  do gf <- getContents 
     let g = parseXmlGrammar gf 
         showfam f = "begin family " ++ f ++ "\n\n"
                     ++ (concatMap toGeniHand t)
                     ++ "end family\n\n"
                     where t = lookupWithDefaultFM g [] f
         outstr = concatMap showfam (keysFM g)
     putStr outstr
\end{code}

