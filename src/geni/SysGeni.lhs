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
\end{code}
}

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

