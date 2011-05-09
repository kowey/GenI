{-# LANGUAGE ForeignFunctionInterface #-}

module Wrapper where

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

foreign export ccall "geni_init"    cGeniInit    :: CString -> IO (Ptr ())
foreign export ccall "geni_realize" cGeniRealize :: Ptr () -> CString -> CString -> IO CString
foreign export ccall "geni_free"    free         :: Ptr a -> IO ()

peekUTF8_CString :: CString -> IO String
peekUTF8_CString = fmap B8.toString . BU.unsafePackCString

cGeniInit :: CString -> IO (Ptr ())
cGeniInit cstr = do
  mfile <- peekCString cstr
  castStablePtrToPtr `fmap` (newStablePtr =<< geniInit mfile)

cGeniRealize :: Ptr () -> CString -> CString -> IO CString
cGeniRealize ptr cx cy = do
  pst <- deRefStablePtr (castPtrToStablePtr ptr)   
  x <- peekUTF8_CString cx
  y <- peekUTF8_CString cy
  newCString =<< geniRealize pst x y

geniInit :: FilePath -> IO ProgState
geniInit mfile = do
  pstRef <- newIORef
             (emptyProgState (setFlagP MacrosFlg mfile emptyParams))
  loadGeniMacros pstRef
  readIORef pstRef

geniRealize :: ProgState    -- ^ GenI handle
            -> String       -- ^ lexicon contents
            -> String       -- ^ semantics
            -> IO String    -- ^ JSON formatted results
geniRealize pst lex sem = do
  l <- either (fail . show) return (lParse "(str)" lex)
  pstRef <- newIORef $ pst { le = l }
  loadTargetSemStr pstRef $ "semantics:[" ++ sem ++ "]"
  --
  (encode . fst3) `fmap` runGeni pstRef simpleBuilder_2p
