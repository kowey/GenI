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

\chapter{Main}

Welcome to the GenI source code.  The main module is where everything
starts from.  If you're trying to figure out how GenI works, the main
action is in Geni and Tags 
(chapters \ref{cha:Geni} and \ref{cha:Tags}).  

\begin{code}
module Main (main) where
\end{code}

\ignore{
\begin{code}
import Data.IORef(newIORef)
import System.Environment(getArgs)

import NLP.GenI.Geni(emptyProgState)
import NLP.GenI.Console(consoleGeni)
import NLP.GenI.Configuration (treatStandardArgs, processInstructions,
                               hasFlagP, BatchDirFlg(..), DisableGuiFlg(..), FromStdinFlg(..),
                               RegressionTestModeFlg(..),
                              )

#ifndef DISABLE_GUI
import NLP.GenI.Gui(guiGeni)
#else
guiGeni = consoleGeni
#endif
\end{code}
}

In figure \ref{fig:code-outline-main} we show what happens from main: First, we
hand control off to either the console or the graphical user interface.  These
functions then do all the business stuff like loading files and figuring out
what to generate.  From there, they invoke the the generation step
\fnref{runGeni} which does surface realisation from A-Z.  Alternately, the
graphical interface could invoke a graphical debugger which also does surface
realisation from A-Z but allows you to intervene, inspect and stop at each
step.

\begin{figure}
\begin{center}
\includegraphics[scale=0.25]{images/code-outline-main}
\label{fig:code-outline-main}
\caption{How the GenI entry point is used}
\end{center}
\end{figure}

\begin{code}
main :: IO ()
main = do       
  args     <- getArgs
  confArgs <- treatStandardArgs args >>= processInstructions
  let pst = emptyProgState confArgs
  pstRef <- newIORef pst
  let batch   = hasFlagP BatchDirFlg confArgs
      console = hasFlagP DisableGuiFlg confArgs
      fromstdin = hasFlagP FromStdinFlg confArgs
      regression = hasFlagP RegressionTestModeFlg confArgs
  if (fromstdin || console || batch || regression)
     then consoleGeni pstRef
     else guiGeni pstRef
\end{code}

% TODO
% Define what is and what is not exported from the modules.  
%      In particular in BTypes take care to export the inspection function 
%      but not the types.
%      Re-write functions in Main as needed.
% Change input in Lexicon and Grammar to allow more than one anchor.
