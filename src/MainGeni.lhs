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
import System(getArgs)

import NLP.GenI.Btypes(Macros)
import NLP.GenI.Geni(emptyProgState, ProgState(gr))
import NLP.GenI.Console(consoleGeni)
import NLP.GenI.Configuration (treatArgs, isGraphical, Params(batchDir),
                               grammarType, GrammarType(PreCompiled),
                              )

#ifndef PRECOMPILED_GRAMMAR
#ifndef DISABLE_GUI
import NLP.GenI.Gui(guiGeni)
#endif {- DISABLE_GUI -}
#endif {- PRECOMPILED_GRAMMAR -}

#ifdef PRECOMPILED_GRAMMAR
import MyGeniGrammar
#endif {- PRECOMPILED_GRAMMAR -}

mPreGrammar :: Maybe Macros
#ifdef PRECOMPILED_GRAMMAR
mPreGrammar = Just myGeniGrammar
#else
mPreGrammar = Nothing
#endif {- PRECOMPILED_GRAMMAR -}

#ifdef PRECOMPILED_GRAMMAR
guiGeni = consoleGeni
#endif {- PRECOMPILED_GRAMMAR -}
#ifdef DISABLE_GUI
guiGeni = consoleGeni
#endif {- DISABLE_GUI -}
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
  confArgs <- treatArgs args
  let pst = case mPreGrammar of
            Nothing -> emptyProgState confArgs
            Just g  -> let cargs = confArgs { grammarType = PreCompiled }
                       in  (emptyProgState cargs) { gr = g }
  pstRef <- newIORef pst
  let notBatch  = null (batchDir confArgs)
      graphical = isGraphical confArgs 
  if (graphical && notBatch) 
     then guiGeni pstRef
     else consoleGeni pstRef
\end{code}

% TODO
% Define what is and what is not exported from the modules.  
%      In particular in BTypes take care to export the inspection function 
%      but not the types.
%      Re-write functions in Main as needed.
% Change input in Lexicon and Grammar to allow more than one anchor.
