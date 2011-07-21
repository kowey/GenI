{-# LANGUAGE ForeignFunctionInterface #-}

module MinimalGenI where

import Control.Exception
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef)
import qualified Data.ByteString.Unsafe as BU
import qualified Data.ByteString.UTF8   as B8

import System.Environment (getArgs)
import System.IO hiding ( getContents, putStrLn, hPutStrLn )
import System.IO.Unsafe
import System.IO.UTF8
import Text.JSON
import Prelude hiding ( getContents, putStrLn )

import NLP.GenI.Configuration
import NLP.GenI.General (fst3)
import NLP.GenI.Geni
import NLP.GenI.GeniParsers ( geniLexicon, runParser, ParseError )
import NLP.GenI.Lexicon ( Lexicon )
import NLP.GenI.Simple.SimpleBuilder
import qualified NLP.GenI.Builder as B

import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.Ptr
import Foreign.StablePtr

-- returns NULL pointer if anything goes wrong
foreign export ccall "geni_init"    cGeniInit    :: CString -> CString -> IO (Ptr ())
-- returns error message on parse errors of input files
foreign export ccall "geni_realize" cGeniRealize :: Ptr () -> CString -> IO CString
foreign export ccall "geni_free"    free         :: Ptr a -> IO ()

peekUTF8_CString :: CString -> IO String
peekUTF8_CString = fmap B8.toString . BU.unsafePackCString

cGeniInit :: CString -> CString -> IO (Ptr ())
cGeniInit cm cl = do
  mfile <- peekCString cm
  lfile <- peekCString cl
  p <- geniInit mfile lfile
  case p of
   Left _   -> return nullPtr
   Right p2 -> castStablePtrToPtr `fmap` newStablePtr p2

cGeniRealize :: Ptr () -> CString -> IO CString
cGeniRealize ptr cx = do
  pst <- deRefStablePtr (castPtrToStablePtr ptr)   
  x <- peekUTF8_CString cx
  newCString =<< geniRealize pst x

geniInit :: FilePath -> FilePath -> IO (Either BadInputException ProgStateRef)
geniInit mfile lfile = do
  pstRef <- newIORef . emptyProgState
                     . setFlagP LexiconFlg lfile
                     . setFlagP MacrosFlg  mfile
                     $ emptyParams
  try $ do loadGeniMacros pstRef
           loadLexicon pstRef
           return pstRef

-- | Print any errors in an JSON error object
geniRealize :: ProgStateRef
             -> String
             -> IO String
geniRealize pstRef sem = do
  me <- geniRealizeI pstRef sem
  return $ case me of
     Left (BadInputException e) -> encode . errorObject . show $ e
     Right p                    -> encode . fst3 $ p

geniRealizeI pstRef sem = try $ do
  loadTargetSemStr pstRef $ "semantics:[" ++ sem ++ "]"
  runGeni pstRef simpleBuilder_2p

errorObject str = toJSObject [ ("error", str) ]
