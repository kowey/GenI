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

import Control.Monad ( liftM, ap )
import Control.Monad.IO.Class ( liftIO )
import Data.Conduit
import Data.Conduit.List hiding ( map, concatMap )
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy as B
import Network.Wai
import Network.HTTP.Types (status200, status400, Header, Ascii)
import qualified Text.JSON as J
import qualified Text.JSON.Pretty as J

import NLP.GenI.Configuration
import NLP.GenI
import NLP.GenI.Simple.SimpleBuilder
import NLP.GenI.Parser ( ParseError )

import NLP.GenI.Server.Flag
import NLP.GenI.Server.Instruction

initialise :: Params -> IO ProgState
initialise confArgs = do
    pstRef   <- newIORef (emptyProgState $ setFlagP FromStdinFlg () confArgs)
    _   <- loadGeniMacros pstRef
    _   <- loadLexicon    pstRef
    readIORef pstRef

data GenReq = Dump | Normal

toGenReq :: Request -> Either String GenReq
toGenReq req =
    case pathInfo req of
        ["dump"] -> Right Dump
        []       -> Right Normal
        xs       -> Left $ "Don't know about path: " ++ T.unpack (T.intercalate "/" xs)

parseInstruction :: J.JSON j => B.ByteString -> Either String j
parseInstruction = J.resultToEither . J.decode . TL.unpack . TL.decodeUtf8

application :: ProgState -> Application
application pst req = do
    bss <- requestBody req $$ consume
    let input = (,) `liftM` toGenReq req `ap` parseInstruction (B.fromChunks bss)
    case input of
      Left e    -> return (err (TL.pack e))
      Right tyj -> uncurry heart tyj
  where
    heart ty j = do
        me <- liftIO (handleRequest pst j)
        case me of
            Right p  -> return (ok ty p)
            Left e   -> return (err . TL.pack $ "parse error: " ++ show e)

-- TODO: what to do about the warnings?
ok :: GenReq -> GeniResults -> Response
ok Dump   j =
     responseLBS status200  [contentType "application/json"] $
         TL.encodeUtf8 $ prettyEncode j
ok Normal j =
     responseLBS status200  [contentType "text/plain"] $
         TL.encodeUtf8 $ showResults (grResults j)

err :: TL.Text -> Response
err x = responseLBS status400 [contentType "text/plain"] (TL.encodeUtf8 x)

showResults :: [GeniResult] -> TL.Text
showResults xs = TL.unlines . concat $
    [ map TL.fromChunks [grRealisations g] | GSuccess g <- xs ]

handleRequest :: ProgState -> ServerInstruction -> IO (Either ParseError GeniResults)
handleRequest pst instr = do
    conf   <- treatArgsWithParams optionsForRequest params (pa pst)
    pstRef <- newIORef (pst { pa = conf })
    let mSemInput = parseSemInput $ "semantics:[" ++ semStr ++ "]"
    case mSemInput of
        Left e         -> return (Left e)
        Right semInput -> do
            -- do the realisation
            let helper builder = fst `fmap` runGeni pstRef semInput builder
            results <- case builderType conf of
                           SimpleBuilder         -> helper simpleBuilder_2p
                           SimpleOnePhaseBuilder -> helper simpleBuilder_1p
            return (Right results)
  where
    params = gParams    instr
    semStr = gSemantics instr

-- ----------------------------------------------------------------------

encodeB :: TL.Text -> B.ByteString
encodeB = TL.encodeUtf8

contentType :: Ascii -> Header
contentType x = ("Content-Type", x)

prettyEncode :: J.JSON a => a -> TL.Text
prettyEncode = TL.pack . J.render . J.pp_value . J.showJSON
