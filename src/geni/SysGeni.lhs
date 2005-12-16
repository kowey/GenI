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

The SysGeni module mainly exists for running GenI as an application bundle
under MacOS X.  We mostly re-export stuff from System.Process, but if we 
are in a MacOS X application bundle, then we add \verb!../Resources/bin!
to the path for all the random crap that we ship with with GenI.

\begin{code}
module SysGeni 
where
\end{code}

\ignore{
\begin{code}
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

