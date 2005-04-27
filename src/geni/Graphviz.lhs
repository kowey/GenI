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

\chapter{Graphviz}

We use Graphviz to visualise the results of our generator 
(derivation and derived trees) as well as any other intermediary steps 
that could benefit from visualisation.  Graphviz converts an abstract
representation of a graph (node foo is connected to node bar, etc.) into a
nicely laid out graphic.  This module contains the code to invoke graphviz and
to convert graphs and trees to its input format.

You can download this (open source) tool at
\url{http://www.research.att.com/sw/tools/graphviz}.

\begin{code}
module Graphviz
where
\end{code}

\ignore{
\begin{code}
import Monad(when)
import System.IO(hPutStrLn, hClose)
import SysGeni(runPiped, awaitProcess)
\end{code}
}

\section{Interface}

We expose one or two functions to directly convert our data structures 
into graphics files.  

\paragraph{GraphvizShow} 
The idea is that data structures which can be visualised with GraphViz
should implement this class.  Note the first argument to graphvizShow is
so that you can parameterise your show function 
(i.e. pass in flags to change the way you show particular object)

FIXME: ideally the flag would be of parameterisable type... but it 
       seems i'll have to enable glasgow-exts to get classes that
       let me do that.  

\begin{code}
type GvParam = Bool
class GraphvizShow a where
  graphvizShow :: GvParam -> a -> String
\end{code}

The conversion process and graphviz invocation itself is in the sections
below.  Note: the dotFile argument allows you to save the intermediary
dot output to a file.  You can pass in the empty string if you don't
want this.

\begin{code}
toGraphviz :: (GraphvizShow a) => GvParam -> a -> String -> String -> IO () 
toGraphviz p x dotFile outputFile =
   graphviz (graphvizShow p x) dotFile outputFile
\end{code}

\section{Invocation}

Calls graphviz. The first argument is a String in graphviz's dot format.
The second is the name of the file graphviz should write the first argument 
to. The third is the name of output the file graphviz should write its graphic
to.  If the second argument is the empty string, then we don't write any 
dot file.

\begin{code}
graphviz:: String -> String -> String -> IO () 
\end{code}

We write the dot String to a temporary file which we then feed to graphviz.
This is avoid complications with fork and pipes.  We use png output even
though it's uglier, because we don't have a wxhaskell widget that can 
display postscript... do we?

\begin{code}
graphviz dot dotFile outputFile = do
   let dotArgs' = ["-Gfontname=courier", 
                   "-Nfontname=courier", 
                   "-Efontname=courier", 
                   "-Tpng", "-o" ++ outputFile ]
       dotArgs = dotArgs' ++ (if (null dotFile) then [] else [dotFile])
   -- putStrLn ("sending to graphviz:\n" ++ dot) 
   Monad.when (not $ null dotFile) $ writeFile dotFile dot
   (pid, _, toGV) <- runPiped "dot" dotArgs Nothing Nothing
   Monad.when (null dotFile) $ do 
     hPutStrLn toGV dot 
     hClose toGV
   awaitProcess pid
\end{code}


