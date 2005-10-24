% GenI surface realiser
% Copyright (C) 2005 Carlos Areces and Eric Kow
%
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation; either version 2
% of the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

\chapter{SysGeni}

The SysGeni module handles everything which is related to interaction with the
operating system.

\begin{code}
module SysGeni 
where
\end{code}

\ignore{
\begin{code}
import System.Exit(ExitCode)
import System.Posix
import System.IO(Handle, BufferMode(..), hSetBuffering)
import System.Directory(setCurrentDirectory)
import qualified System.Process as S

import Data.List(intersperse, isSuffixOf)
import General(slash)

#ifdef __GLASGOW_HASKELL__
import Foreign
import Foreign.C
import Control.Monad
#include "ghcconfig.h"
#endif
\end{code}
}

\section{Running a process}

We mostly re-export stuff from System.Process.  

\begin{code}
waitForProcess = S.waitForProcess
\end{code}

But one thing special we need to do for Macs is to detect if we're
running from an application bundle.  If we are, we assume that any
processes we want to run are in \texttt{../Resources/bin}.

\begin{code}
#ifdef darwin_TARGET_OS 
runInteractiveProcess cmd args x y = do
  dirname <- getProgDirName
  -- detect if we're in an .app bundle, i.e. if 
  -- we are running from something.app/Contents/MacOS
  let insertSlashes p = (concat $ intersperse slash p)
      appBundle = (insertSlashes p) ++ slash
        where p = [ ".app", "Contents", "MacOS" ]
      resBinCmd = dirname ++ (insertSlashes p)
        where p = [ "..", "Resources", "bin", cmd ]
  -- if we're in an .app bundle, we should prefix the
  -- path with ../Resources/bin
  let cmd2 = if appBundle `isSuffixOf` dirname 
             then resBinCmd else cmd
  S.runInteractiveProcess cmd2 args x y 
#else 
-- if not on a Mac
runInteractiveProcess = S.runInteractiveProcess
#endif
\end{code}

\paragraph{Process helpers}

\begin{code}
foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

getProgDirName :: IO String
getProgDirName = 
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
     getProgArgv p_argc p_argv
     argv <- peek p_argv
     s <- peekElemOff argv 0 >>= peekCString
     return $ dirname s
  where
   dirname :: String -> String
   dirname f = reverse $ dropWhile (not.isPathSeparator) $ reverse f
   isPathSeparator :: Char -> Bool
   isPathSeparator '/'  = True
#ifdef mingw32_TARGET_OS 
   isPathSeparator '\\' = True
#endif
   isPathSeparator _    = False
\end{code}

\section{Old System Stuff}

For some software, I cannot seem to get rid of this function. 
\fnreflite{runInteractiveProcess} doesn't seem to do what I want.

\paragraph{runPiped}

To run a command, we implement a simple function to fork the process and make a
system call to the command in the child process.  Note, I stole this function
from DaVinci.hs by Sven Panne.  Also, there is a much simpler
\texttt{runProcess} function in the Posix package, but it doesn't return a pid
for us to wait on.

\begin{code}
runPiped :: FilePath                        -- Command
         -> [String]                        -- Arguments
         -> Maybe [(String, String)]        -- Environment
         -> Maybe FilePath                  -- Working directory    
         -> IO (ProcessID, Handle, Handle)  -- (pid, fromChild, toChild)
\end{code}

\begin{code}
runPiped path args env dir = do
   (rd1, wd1) <- createPipe
   (rd2, wd2) <- createPipe
   let childWork = do maybe (return ()) setCurrentDirectory dir
                      dupTo rd1 stdInput 
                      dupTo wd2 stdOutput 
                      mapM_ closeFd [rd1, wd1, rd2, wd2]
                      executeFile path True args env
                      ioError (userError "runPiped")

       parentWork pid = do -- parent
                           mapM_ closeFd [rd1, wd2]
                           fromChild <- fdToHandle rd2
                           toChild   <- fdToHandle wd1
                           hSetBuffering fromChild LineBuffering
                           hSetBuffering toChild   LineBuffering
                           return (pid, fromChild, toChild)
   do pid <- forkProcess childWork
      parentWork pid 
\end{code} 

Waits for a process to finish.  I don't really understand all this process
stuff, but i suspect this is blocking.

\begin{code}
awaitProcess :: ProcessID -> IO (Maybe ExitCode)
awaitProcess pid = do 
      s <- getProcessStatus False True pid 
      let ret = case s of 
                  Nothing -> Nothing
                  Just s2 -> case s2 of Exited c -> Just c; _ -> Nothing
      return ret 
\end{code}

