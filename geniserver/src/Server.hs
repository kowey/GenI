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

import Data.List
import Data.IORef (newIORef, readIORef, modifyIORef)
import Network (withSocketsDo, listenOn, accept, Socket)
import System (getArgs)
import System.IO
import System.Posix.Signals (installHandler, sigPIPE, Handler(Ignore))

import NLP.GenI.Configuration
import NLP.GenI.General (fst3)
import NLP.GenI.Geni
import NLP.GenI.Simple.SimpleBuilder
import NLP.GenI.CkyEarley.CkyBuilder
import qualified NLP.GenI.Builder as B

import ClientServer (hGetBeginEnd, hardCodedPort)

main :: IO ()
main = withSocketsDo $
 do -- ignore SIGPIPE so that we don't just die if the client
    -- is available when we try to write back to it
    installHandler sigPIPE Ignore Nothing
    confArgs <- treatStandardArgs =<< getArgs
    pstRef <- newIORef (emptyProgState $ setFlagP FromStdinFlg () confArgs)
    loadEverything pstRef
    pst  <- readIORef pstRef
    sock <- listenOn hardCodedPort
    listen sock pst

listen :: Socket -> ProgState -> IO ()
listen sock pst =
 do pstRef <- newIORef pst
    (h,_,_) <- accept sock
    -- do a task
    mparams <- hGetBeginEnd "params" h
    msem    <- hGetBeginEnd "semantics" h
    -- any errors? (Left err monad)
    let mtask = do p <- mparams
                   s <- msem
                   return (p, unlines s)
    case mtask of
      Left err   -> hPutStrLn stderr (show err)
      Right (params, semStr) ->
       ignoringErrors $
       do conf <- treatStandardArgsWithParams params (pa pst)
          modifyIORef pstRef (\p -> p { pa = conf })
          loadTargetSemStr pstRef semStr
          -- do the realisation
          let helper builder = fmap (sort.fst3) $ runGeni pstRef builder
          sentences <- case builderType conf of
                         NullBuilder   -> helper B.nullBuilder
                         SimpleBuilder -> helper simpleBuilder_2p
                         SimpleOnePhaseBuilder -> helper simpleBuilder_1p
                         CkyBuilder    -> helper ckyBuilder
                         EarleyBuilder -> helper earleyBuilder
          -- return the results
          hPutStrLn h "begin responses"
          hPutStr h $ unlines $ map fst sentences
          hPutStrLn h "end responses"
    -- close shop and start over
    ignoringErrors $ hClose h
    listen sock pst -- the original
 where
  ignoringErrors job = job `catch` \err -> hPutStrLn stderr (show err)
