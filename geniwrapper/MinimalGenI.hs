{-# LANGUAGE ForeignFunctionInterface #-}

module MinimalGenI where

import Control.Exception
import Control.Monad ( when, unless )
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
import NLP.GenI.GeniParsers ( geniLexicon, geniFeats, runParser, ParseError, tillEof )
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
foreign export ccall "geni_realize" cGeniRealize :: Ptr () -> CString -> CString -> CString -> IO CString
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

cGeniRealize :: Ptr () -> CString -> CString -> CString -> IO CString
cGeniRealize ptr cx cy cz = do
  pst <- deRefStablePtr (castPtrToStablePtr ptr)
  x <- peekUTF8_CString cx
  y <- peekUTF8_CString cy
  z <- peekUTF8_CString cz
  newCString =<< geniRealize pst x y z

geniInit :: FilePath -> FilePath -> IO (Either BadInputException ProgStateRef)
geniInit mfile lfile = do
  pstRef <- newIORef . emptyProgState
                     . setFlagP LexiconFlg lfile
                     . setFlagP MacrosFlg  mfile
                     $ emptyParams
  try $ do loadGeniMacros pstRef
           when (not (null lfile)) $ loadLexicon pstRef >> return ()
           return pstRef

-- | Print any errors in an JSON error object
geniRealize :: ProgStateRef
             -> String -- ^ lexicon
             -> String -- ^ semantics
             -> String -- ^ root feature
             -> IO String
geniRealize pstRef lex sem rf = do
  me <- geniRealizeI pstRef lex sem rf
  return $ case me of
     Left (BadInputException d e) -> encode . errorObject $ d ++ " parse error: " ++ show e
     Right p                      -> encode . fst3 $ p

geniRealizeI pstRef lex sem rf = try $ do
  unless (null lex) $ do
     l <- loadFromString pstRef "lexicon" lex
     let _ = l :: Lexicon
     return ()
  loadTargetSemStr pstRef $ "semantics:[" ++ sem ++ "]"
  r <- tryParse geniFeats "root feature" rf
  modifyIORef pstRef $ \p -> p { pa = setFlagP RootFeatureFlg r (pa p) }
  runGeni pstRef simpleBuilder_2p

tryParse p descr str =
  case runParser (tillEof p) () "" str of
  Left  err -> throwIO (BadInputException descr err)
  Right res -> return res

errorObject str = toJSObject [ ("error", str) ]
