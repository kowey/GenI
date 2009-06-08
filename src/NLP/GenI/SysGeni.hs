-- GenI surface realiser
-- Copyright (C) 2005 Carlos Areces and Eric Kow
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

{-# LANGUAGE ForeignFunctionInterface #-}

-- | The SysGeni module mainly exists for running GenI as an application bundle
--   under MacOS X.  We mostly re-export stuff from System.Process, but if we
--   are in a MacOS X application bundle, then we add @../Resources/bin@
--   to the path for the miscellaneous resources that we ship with with GenI.
module NLP.GenI.SysGeni
where

import qualified System.Process as S

import Data.List (isSuffixOf)
import System.FilePath
import System.IO (Handle)
import System.Exit (ExitCode)

#ifdef __GLASGOW_HASKELL__
import Foreign
import Foreign.C
import Control.Monad
#include "ghcconfig.h"
#endif

-- * Running a process

waitForProcess :: S.ProcessHandle -> IO ExitCode
waitForProcess = S.waitForProcess

-- | One thing special we need to do for Macs is to detect if we're
--   running from an application bundle.  If we are, we assume that any
--   processes we want to run are in @../Resources/bin@.
runInteractiveProcess :: String -> [String]
                      -> Maybe FilePath
                      -> Maybe [(String, String)]
                      -> IO (Handle, Handle, Handle, S.ProcessHandle)
runInteractiveProcess cmd args x y = do
  dirname <- getProgDirName
  -- detect if we're in an .app bundle, i.e. if 
  -- we are running from something.app/Contents/MacOS
  let appBundle = ".app/Contents/MacOS/"
      resBinCmd = "../Resources/bin" </> cmd
  -- if we're in an .app bundle, we should prefix the
  -- path with ../Resources/bin
  let cmd2 = if appBundle `isSuffixOf` dirname 
             then resBinCmd else cmd
  S.runInteractiveProcess cmd2 args x y 

-- * Process helpers

foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

getProgDirName :: IO String
getProgDirName = 
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
     getProgArgv p_argc p_argv
     argv <- peek p_argv
     s <- peekElemOff argv 0 >>= peekCString
     return $ takeDirectory s
