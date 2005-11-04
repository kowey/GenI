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

\chapter{ExtractTestCases}

This is a standalone program that provides a simple interface to the
test suite parser. 

The idea is that you give it a directory.  For each test case in the
suite, it creates a subdirectory (with the test case name) and in 
that subdirectory, it creates two files containing the semantics and
the sentences.

The purpose of this program is to serve as helper module for tools 
that batch-test GenI.

\begin{code}
module Main (main) where

import Btypes
import GeniParsers(geniTestSuite)

import Control.Monad(when)
import System.Directory
import System.Environment
import System.Exit(exitFailure)
import System.IO
import Text.ParserCombinators.Parsec 
\end{code}

\begin{code}
type TestCase = (String,SemInput,[String])

main :: IO ()
main = 
 do -- read the command line arguments
    pname <- getProgName
    argv  <- getArgs
    let usage = "usage: " ++ pname ++ " testsuite outputdir"
    when (length argv /= 2) $ do
      hPutStrLn stderr usage 
      exitFailure
    let tfilename = argv !! 0
        outdir    = argv !! 1
    -- parse the test suite
    parsed <- parseFromFile geniTestSuite tfilename 
    suite  <- case parsed of
               Left  err     -> fail (show err)
               Right entries -> return entries 
    -- write stuff to the output directory
    createDirectoryIfMissing False outdir
    mapM (createSubdir outdir) suite
    return ()
    
createSubdir :: String -> TestCase -> IO ()
createSubdir outdir testcase = 
 do --  
    let slash x y = x ++ "/" ++ y
        (name, semres, sent) = testcase
        (sem, res) = semres
        subdir = outdir `slash` name
    createDirectoryIfMissing False subdir
    --
    let semanticsStr = "semantics:" ++ showSem sem 
          ++ (if null res then "" else r)
         where r = "\nrestrictors: [" ++ showPairs res ++ "]"
        sentencesStr = unlines sent
    writeFile (subdir `slash` "semantics") semanticsStr 
    writeFile (subdir `slash` "sentences") sentencesStr 
\end{code}


