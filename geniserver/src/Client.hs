{-
GenIClientServer
Copyright (C) 2007 Eric Kow

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

module Main (main) where

import Network (connectTo, withSocketsDo)
import System (getArgs)
import System.IO

import ClientServer (hardCodedPort, hGetBeginEnd)

main :: IO ()
main = withSocketsDo $
 do args <- getArgs
    h <- connectTo "" hardCodedPort -- "127.0.0.1" hardCodedPort
    hSetBuffering h LineBuffering
    hPutStrLn h "begin params"
    hPutStrLn h $ unlines args
    hPutStrLn h "end params"
    hPutStrLn h "begin semantics"
    hPutStrLn h =<< getContents
    hPutStrLn h "end semantics"
    mres <- hGetBeginEnd "responses" h
    case mres of
     Left err  -> hPutStrLn stderr err
     Right res -> putStr $ unlines res
