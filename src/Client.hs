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

import Network (connectTo, withSocketsDo, PortID(UnixSocket, PortNumber))
import System (getArgs)
import System.IO

import NLP.GenI.ClientServer (socketPath, hGetBeginEnd, hGetNonEmptyLine)

main :: IO ()
main = withSocketsDo $
 do args <- getArgs
    h <- connectTo "" (UnixSocket socketPath) -- "127.0.0.1" (PortNumber 2035)
    hSetBuffering h LineBuffering
    hPutStrLn h "begin task"
    hPutStrLn h $ unlines args
    hPutStrLn h "end task"
    mres <- hGetBeginEnd "responses" h
    case mres of
     Left err  -> hPutStrLn stderr err
     Right res -> putStrLn $ unlines res
