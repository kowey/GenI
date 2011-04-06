{-# LANGUAGE DeriveDataTypeable #-}
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

import Control.Applicative hiding (empty)
import Network (connectTo, withSocketsDo, PortID(..))
import System.Environment (getArgs)
import System.IO
import qualified System.IO.UTF8 as UTF8
import Text.JSON
import NLP.GenI.Geni ( GeniResult(..) )
import ClientServer (ServerInstruction(..), hGetBlock, hPutBlock)
import System.Console.CmdArgs
import Data.Version ( showVersion )
import Paths_geniserver ( version )

data Client = Connect { hostname :: String
                      , port     :: Int
                      }
            | Socket { socket :: String }
            | Dump
 deriving (Show, Data, Typeable)

clientCfg :: Client
clientCfg = modes
            [ Connect { hostname = def &= help "hostname" &= argPos 0
                      , port     = def &= help "port INT"
                      }
            , Socket { socket = def &= help "Unix socket at PATH" &= argPos 0 
                     }
            , Dump &= help "Dump request to stdout (for debugging)"
            ] &= help ("geniclient " ++ showVersion version)

main :: IO ()
main = withSocketsDo $
 do config <- cmdArgs clientCfg
    instructions <- ServerInstruction [] <$> UTF8.getContents
    case config of
      Dump -> hPutBlock stdout instructions
      Socket s    -> sendTo instructions =<< connectTo "" (UnixSocket s)
      Connect h p -> sendTo instructions =<< connectTo h (PortNumber (fromIntegral p))

sendTo instructions h =
 do hSetBuffering h NoBuffering
    hPutBlock h instructions
    hFlush h
    mres <- hGetBlock h
    case mres of
     Left err  -> hPutStrLn stderr err
     Right res -> UTF8.putStr . unlines . concatMap grRealisations $ res
