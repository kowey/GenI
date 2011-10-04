{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-
geniserver
Copyright (C) 2011 Eric Kow (on behalf of SRI)

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

import Network.Wai.Handler.Warp (run)
import Data.Maybe ( fromMaybe )
import NLP.GenI.Configuration
import NLP.GenI.Server
import NLP.GenI.Server.Flags
import System.Environment

main :: IO ()
main = do
  pname    <- getProgName
  confArgs <- treatArgs serverOptions =<< getArgs
  let has = flip hasFlagP confArgs
  case () of
   _ | has HelpFlg -> putStrLn (usage serverOptionsSections pname)
     | otherwise   -> startServer confArgs

startServer :: Params -> IO ()
startServer confArgs = do
  pst <- initialise confArgs
  run port (application pst)
 where
  port = fromMaybe defaultPort (getFlagP PortFlg confArgs)

defaultPort :: Int
defaultPort = 4364
