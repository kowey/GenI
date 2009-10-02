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

import Control.Applicative
import Network (connectTo, withSocketsDo)
import System.Environment (getArgs)
import System.IO
import qualified System.IO.UTF8 as UTF8
import Text.JSON
import NLP.GenI.Geni ( GeniResult(..) )
import ClientServer (hardCodedPort, ServerInstruction(..), hGetBlock, hPutBlock)

main :: IO ()
main = withSocketsDo $
 do instructions <- ServerInstruction <$> getArgs <*> UTF8.getContents
    h <- connectTo "" hardCodedPort -- "127.0.0.1" hardCodedPort
    hSetBuffering h NoBuffering
    hPutBlock h (encode instructions)
    hFlush h
    mres <- hGetBlock h
    case mres of
     Left err  -> hPutStrLn stderr err
     Right res -> UTF8.putStr . unlines . concatMap grRealisations $ res
