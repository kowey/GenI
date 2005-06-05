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

This module's sole job is to decide between the text/graphical 
interface.

TODO 
\begin{enumerate}
\item Define what is and what is not exported from the modules.  
      In particular in BTypes take care to export the inspection function 
      but not the types.
      Re-write functions in Main as needed.
\item Change input in Lexicon and Grammar to allow more than one anchor.
\item Keys used in Tags are specially bad for Pn, perhaps they can be improved.
\end{enumerate}

\begin{code}
module Main (main) where
\end{code}

\ignore{
\begin{code}
import Data.IORef(readIORef)

import Geni(initGeni, pa, batchPa)
import Gui(guiGenerate)
import Console(consoleGenerate)
import Configuration(isGraphical, isBatch)
\end{code}
}

\begin{code}
main :: IO ()
main = do       
  pst <- initGeni
  mst <- readIORef pst
  let headPa   = pa mst
  let notBatch  = (  ((length $ batchPa mst) == 1) 
                  && (not $ isBatch headPa))
      graphical = isGraphical headPa
  if (graphical && notBatch) 
     then guiGenerate pst
     else consoleGenerate pst
\end{code}
