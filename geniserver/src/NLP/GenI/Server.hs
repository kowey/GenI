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

module NLP.GenI.Server where

import Control.Monad.IO.Class ( liftIO )
import Data.IORef
import Network.Wai
import Network.HTTP.Types (statusOK, status400)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.Enumerator.Binary as EB
import qualified Text.JSON as J
import qualified Text.JSON.Pretty as J

import NLP.GenI.Configuration
import NLP.GenI.General (fst3)
import NLP.GenI.Geni
import NLP.GenI.Simple.SimpleBuilder

import NLP.GenI.Server.Flags
import NLP.GenI.Server.Instruction


initialise :: Params -> IO ProgState
initialise confArgs = do
  pstRef   <- newIORef (emptyProgState $ setFlagP FromStdinFlg () confArgs)
  _   <- loadGeniMacros pstRef
  _   <- loadLexicon    pstRef
  readIORef pstRef

application :: ProgState -> Application
application pst _ = do
  bs     <- EB.consume
  let mj = J.decode (B.toString bs)
  case mj of
    J.Ok j    -> do
      answer <- liftIO $ handleRequest pst (gParams j) (gSemantics j)
      return $ responseLBS statusOK  [("Content-Type", "application/json")] $ B.fromString (prettyEncode answer)
    J.Error s ->
      return $ responseLBS status400 [("Content-Type", "text/plain")]  $ B.concat [ B.fromString s, "\n", bs ]

handleRequest :: ProgState -> [String] -> String -> IO [GeniResult]
handleRequest pst params semStr = do
  conf   <- treatArgsWithParams optionsForRequest params (pa pst)
  pstRef <- newIORef (pst { pa = conf })
  loadTargetSemStr pstRef $ "semantics:[" ++ semStr ++ "]"
  -- do the realisation
  let helper builder = fst3 `fmap` runGeni pstRef builder
  results <- case builderType conf of
               SimpleBuilder -> helper simpleBuilder_2p
               SimpleOnePhaseBuilder -> helper simpleBuilder_1p
  return results

prettyEncode :: J.JSON a => a -> String
prettyEncode = J.render . J.pp_value . J.showJSON
