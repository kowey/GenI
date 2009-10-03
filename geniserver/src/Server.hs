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

import Data.IORef (newIORef, readIORef, modifyIORef)
import Network (withSocketsDo, listenOn, accept, Socket, PortID(..))
import System.Environment (getArgs)
import System.IO hiding ( getContents, putStrLn, hPutStrLn )
import System.IO.UTF8
import System.Posix.Signals (installHandler, sigPIPE, Handler(Ignore))
import Text.JSON
import Prelude hiding ( getContents, putStrLn )

import NLP.GenI.Configuration
import NLP.GenI.General (fst3)
import NLP.GenI.Geni
import NLP.GenI.Simple.SimpleBuilder
import qualified NLP.GenI.Builder as B
import ClientServer (getPort, ServerInstruction(..),hGetBlock,hPutBlock)


main :: IO ()
main = withSocketsDo $
 do -- ignore SIGPIPE so that we don't just die if the client
    -- is available when we try to write back to it
    installHandler sigPIPE Ignore Nothing
    confArgs <- treatArgs myOptions =<< getArgs
    pstRef <- newIORef (emptyProgState $ setFlagP FromStdinFlg () confArgs)
    case getFlagP ServerInputFlg confArgs of
      Nothing        -> fail $ "Need --listen stdin|filename|port"
      Just FromStdin ->
       do loadEverything pstRef
          minstructions <- hGetBlock stdin
          case minstructions of
            Left err -> fail err
            Right (ServerInstruction params semStr) ->
              putStrLn . encode =<< handleRequest pstRef params semStr
      Just (FromPort port) ->
       do loadEverything pstRef
          pst  <- readIORef pstRef
          sock <- listenOn port
          listen sock pst

listen :: Socket -> ProgState -> IO ()
listen sock pst =
 do pstRef <- newIORef pst
    (h,_,_) <- accept sock
    hSetBuffering h NoBuffering
    -- do a task
    contents <- hGetBlock h
    let minstructions = contents
    -- any errors? (Left err monad)
    case minstructions of
      Left err   -> hPutStrLn stderr (show err)
      Right (ServerInstruction params semStr) ->
       ignoringErrors $ do
          results <- handleRequest pstRef params semStr
          hPutBlock h results
          hFlush h
    -- close shop and start over
    ignoringErrors $ hClose h
    listen sock pst -- the original
 where
  ignoringErrors job = job `catch` \err -> hPutStrLn stderr (show err)

handleRequest pstRef params semStr =
  do pst <- readIORef pstRef
     conf <- treatArgsWithParams optionsForStandardGenI params (pa pst)
     modifyIORef pstRef (\p -> p { pa = conf })
     loadTargetSemStr pstRef $ "semantics:[" ++ semStr ++ "]"
     -- do the realisation
     let helper builder = fst3 `fmap` runGeni pstRef builder
     results <- case builderType conf of
                  NullBuilder   -> helper B.nullBuilder
                  SimpleBuilder -> helper simpleBuilder_2p
                  SimpleOnePhaseBuilder -> helper simpleBuilder_1p
     return results

-- ----------------------------------------------------------------------

myOptions :: [OptDescr Flag]
myOptions = optionsForStandardGenI ++ optionsForServer

optionsForServer :: [OptDescr Flag]
optionsForServer =
  [ Option [] ["listen"] (reqArg ServerInputFlg toServerInput "FILE/PORT")
      "file name (Unix socket), port or 'stdin'"
  ]

data ServerInputFlg = ServerInputFlg ServerInput deriving (Eq, Show, Typeable)

data ServerInput = FromStdin | FromPort PortID
  deriving (Eq, Show, Typeable)

toServerInput :: String -> ServerInput
toServerInput "stdin" = FromStdin
toServerInput x = FromPort (getPort x)
