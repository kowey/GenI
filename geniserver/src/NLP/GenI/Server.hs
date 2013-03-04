{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
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

import Control.Applicative ( (<$>) )
import Control.Monad.Trans.Error ( runErrorT )
import Control.Monad.IO.Class ( liftIO )
import Data.Int
import Data.IORef
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy as B

import Snap.Core
import qualified Text.JSON as J
import qualified Text.JSON.Pretty as J

import NLP.GenI.Configuration
import NLP.GenI
import NLP.GenI.LexicalSelection ( CustomSem(..) )
import NLP.GenI.Simple.SimpleBuilder
import NLP.GenI.Pretty
import NLP.GenI.Server.Flag
import NLP.GenI.Server.Instruction
import qualified NLP.GenI.Configuration as G

initialise :: G.Params -> IO ProgState
initialise confArgs = do
    pstRef   <- newIORef (emptyProgState $ setFlag FromStdinFlg () confArgs)
    _   <- loadGeniMacros pstRef
    _   <- loadLexicon    pstRef
    readIORef pstRef

data GenReq = Dump | Normal

parseInstruction :: J.JSON j => B.ByteString -> Either String j
parseInstruction = J.resultToEither . J.decode . TL.unpack . TL.decodeUtf8

application :: Int64 -- ^ maximum request size (see 'defaultReqMaxSize')
            -> ProgState
            -> CustomSem sem
            -> Snap ()
application reqMaxSz pst wrangler =
    route [ ("dump", handle Dump)
          , (""    , handle Normal)
          ]
  where
    handle ty = do
        bss <- readRequestBody reqMaxSz
        let input = parseInstruction bss
        case input of
            Left e  -> err (T.pack e)
            Right j -> do
                me <- liftIO (handleRequest pst wrangler j)
                case me of
                    Right p  -> ok ty p
                    Left e   -> err e

ok :: GenReq -> GeniResults -> Snap ()
ok Dump j = do
    modifyResponse (setContentType "application/json")
    writeText $ prettyEncode j
ok Normal j = do
    modifyResponse (setContentType "text/plain")
    writeText $ showResults (grResults j)

err :: T.Text -> Snap ()
err x = do
     modifyResponse (setResponseCode 400)
     writeText x
     withResponse finishWith

-- ----------------------------------------------------------------------
--
-- ----------------------------------------------------------------------

showResults :: [GeniResult] -> T.Text
showResults xs = T.unlines . concat $
    [ grRealisations g | GSuccess g <- xs ]

handleRequest :: ProgState -> CustomSem sem -> ServerInstruction -> IO (Either Text GeniResults)
handleRequest pst wrangler instr = do
    conf   <- treatArgsWithParams optionsForRequest params (pa pst)
    case customSemParser wrangler semStr of
        Left e         -> return (Left e)
        Right csem -> do
            -- do the realisation
            let helper builder = simplifyResults <$> (runErrorT $ runGeni pst wrangler builder csem)
            results <- case getBuilderType conf of
                           SimpleBuilder         -> helper simpleBuilder_2p
                           SimpleOnePhaseBuilder -> helper simpleBuilder_1p
            return (Right results)
  where
    params = gParams    instr
    semStr = wrapSem . T.pack $ gSemantics instr
    wrapSem (T.strip -> x) =
        if "semantics:[" `T.isInfixOf` x
           then x
           else "semantics:" <> squares x

-- ----------------------------------------------------------------------

prettyEncode :: J.JSON a => a -> T.Text
prettyEncode = T.pack . J.render . J.pp_value . J.showJSON
