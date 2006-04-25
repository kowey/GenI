% GenI surface realiser
% Copyright (C) 2005 Carlos Areces and Eric Kow
%
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation; either version 2
% of the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

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
import System (ExitCode(ExitFailure), 
               exitWith, getArgs, getProgName)
import System.IO(getContents)
import Text.ParserCombinators.Parsec

import NLP.GenI.Btypes (ifamname, iword, Macros)
import NLP.GenI.General (ePutStrLn)
import NLP.GenI.GeniParsers (geniMacros)
import NLP.GenI.Treeprint (toGeniHand, hsShow, hsLongList)
import NLP.GenI.Tags (TagElem)

import NLP.GenI.Converter.ReadTagml (readTagmlMacros)
\end{code}
}

\begin{code}
main :: IO ()
main =
 do args <- getArgs
    progname <- getProgName
    case args of
      ["-f", f, "-t", t] -> readMacros f >>= writeMacros t
      _ -> showUsage progname
 where
  showUsage p =
    do ePutStrLn ("usage: " ++ p ++ " -f [tagml|geni] -t [haskell|geni] < input > output")
       exitWith (ExitFailure 1)
\end{code}

\begin{code}
readMacros :: String -> IO Macros
readMacros f =
 do lf <- getContents
    case f of
     "tagml" -> case readTagmlMacros lf of
                Left err -> fail err
                Right  c -> return c
     "geni"  -> case parse geniMacros "" lf of
                Left err -> fail (show err)
                Right  c -> return c
     _       -> fail ("Unknown -f type: " ++ f)

writeMacros :: String -> Macros -> IO ()
writeMacros t tes =
 putStrLn $ case t of
            "haskell" -> unlines $ [ "module MyGeniGrammar where"
                                   , "import Data.Tree"
                                   , "import qualified Data.Map"
                                   , "import Btypes"
                                   , "import Tags"
                                   , "myGeniGrammar = " ++ hsLongList tes ]
            "geni"    -> toGeniHand tes
            _         -> fail ("Unknown -t type" ++ t)
\end{code}

%We know how to convert three things
%\begin{enumerate}
%\item lexicon - a TAGML lexicon (unofficial and unused)
%\item macros  - a macros file of unanchored trees
%\item trees   - the output of a tree anchoring module (section \ref{sec:xmg_selection})
%\end{enumerate}
%
%\begin{code}
%convertLexicon :: IO ()
%convertLexicon = 
%  do lf <- getContents 
%     let lex = parseXmlLexicon lf
%         showlex l = (iword l) 
%                     ++ " " ++ (icategory l) ++ "\n"
%                     ++ " " ++ (ifamname l) ++ "\n"
%         outstr    = concatMap showlex lex
%     putStr outstr 
%
%convertMacros :: IO ()
%convertMacros = 
%  do gf <- getContents 
%     let g = readTagmlMacros gf 
%     putStr $ concatMap toGeniHand g
%\end{code}
