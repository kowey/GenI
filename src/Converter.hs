{-
GenI surface realiser
Copyright (C) 2005 Carlos Areces and Eric Kow

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
-}

-- This module is meant to act as a standalone program which serves
-- strictly as a converter between the formats recognised by GenI.

module Main (main) where

import System (ExitCode(ExitFailure), exitWith, getArgs, getProgName)
import System.IO(getContents)
import Text.ParserCombinators.Parsec

import NLP.GenI.Btypes (Macros)
import NLP.GenI.General (ePutStrLn)
import NLP.GenI.GeniParsers (geniMacros)
import NLP.GenI.Treeprint (toGeniHand, hsLongList)
import NLP.GenI.Converter.ReadTagml (readTagmlMacros)

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
            "geni"    -> unlines $ map toGeniHand tes
            _         -> fail ("Unknown -t type" ++ t)
