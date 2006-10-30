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

module Main (main) where

import Data.IORef (newIORef, readIORef, modifyIORef)
import Network (withSocketsDo, listenOn, accept, PortID(UnixSocket), Socket)
import System (getArgs)
import System.IO
import System.Posix.Signals (installHandler, sigPIPE, Handler(Ignore))

import NLP.GenI.Configuration ( treatArgs, treatArgsWithParams
                              , setFlagP, ServerModeFlg(..) )
import NLP.GenI.Console (runTestCaseOnly)
import NLP.GenI.Geni (loadGrammar, loadTestSuite, emptyProgState, ProgState(pa))
import NLP.GenI.ClientServer (hGetBeginEnd, socketPath)

main :: IO ()
main = withSocketsDo $
 do -- ignore SIGPIPE so that we don't just die if the client
    -- is available when we try to write back to it
    installHandler sigPIPE Ignore Nothing
    --
    confArgs <- treatArgs =<< getArgs
    let pst = (emptyProgState $ setFlagP ServerModeFlg () confArgs)
    pstRef <- newIORef pst
    loadGrammar pstRef
    pst2 <- readIORef pstRef
    sock <- listenOn (UnixSocket socketPath) -- (PortNumber 2035) --
    listen sock pst2

listen :: Socket -> ProgState -> IO ()
listen sock pst =
 do pstRef <- newIORef pst
    (h,_,_) <- accept sock
    -- do a task
    mtask <- hGetBeginEnd "task" h
    case mtask of
      Left err   -> hPutStrLn stderr (show err)
      Right task ->
       do conf <- treatArgsWithParams task (pa pst)
          modifyIORef pstRef (\p -> p { pa = conf })
          loadTestSuite pstRef
          (sentences, _) <- runTestCaseOnly pstRef
          hPutStrLn h "begin responses"
          hPutStrLn h $ unlines sentences
          hPutStrLn h "end responses"
    -- close shop and start over
    (hClose h `catch` \err -> hPutStrLn stderr (show err))
    listen sock pst -- the original
