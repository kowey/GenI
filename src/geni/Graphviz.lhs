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
import Control.Monad(when)
import Data.List(intersperse)
import Data.Tree
import System.IO(hPutStrLn, hClose)
import System.Exit(ExitCode)

import SysGeni(waitForProcess, runInteractiveProcess)
\end{code}
}

\section{Interface}

We expose one or two functions to directly convert our data structures 
into graphics files.  

\paragraph{GraphvizShow} 
The idea is that data structures which can be visualised with GraphViz
should implement this class.  Note the first argument to graphvizShow is
so that you can parameterise your show function 
(i.e. pass in flags to change the way you show particular object).
Note that by default, all graphs are treated as directed graphs.  You
can hide this by turning off edge arrows.

\begin{code}
class GraphvizShow flag b where
  graphvizShow            :: flag -> b -> String
  -- flag -> prefix -> item -> gv output
  graphvizShowAsSubgraph  :: flag -> String -> b -> String 
  graphvizLabel           :: flag -> b -> String
  graphvizParams          :: flag -> b -> [String] 

  --- default implementations 
  graphvizShow f b  = 
    let l = graphvizLabel f b
    in "digraph {\n" 
       ++ (unlines $ graphvizParams f b)
       ++ graphvizShowAsSubgraph f "_" b ++ "\n"
       ++ (if null l then "" else " label = \"" ++ l ++ "\";\n")
       ++ "}"
  graphvizLabel _ _ = ""
  graphvizParams _ _ = []

class GraphvizShowNode flag b where
  graphvizShowNode :: flag -> String -> b -> String
\end{code}

The conversion process and graphviz invocation itself is in the sections
below.  Note: the dotFile argument allows you to save the intermediary
dot output to a file.  You can pass in the empty string if you don't
want this.

\begin{code}
toGraphviz :: (GraphvizShow f a) => f -> a -> String -> String -> IO ExitCode 
toGraphviz p x dotFile outputFile = do
   graphviz (graphvizShow p x) dotFile outputFile
\end{code}

\subsection{Useful helper functions}

\begin{code}
gvNewline :: String
gvNewline  = "\\n"

gvUnlines :: [String] -> String
gvUnlines = concat . (intersperse gvNewline)

gvSubgraph :: String -> String
gvSubgraph g = "subgraph {\n" ++ g ++ "}\n"

-- | The Graphviz string for a node.  Note that we make absolutely no
-- effort to escape any characters for you; so if you need to protect
-- anything from graphviz, you're on your own
gvNode :: String                 -- ^ the node name
            -> String            -- ^ the label (may be empty)
            -> [(String,String)] -- ^ any other parameters
            -> String
gvNode name label params =  
  " " ++ name ++ " " ++ (gvLabelAndParams label params) ++ "\n"

-- | The Graphviz string for a connection between two nodes.  
-- Same disclaimer as 'gvNode' applies.
gvEdge :: String  -- ^ the 'from' node
            -> String  -- ^ the 'to' node
            -> String  -- ^ the label (may be empty)
            -> [(String,String)] -- ^ any other parameters 
            -> String
gvEdge from to label params = 
  " " ++ from ++ " -> " ++ to ++ (gvLabelAndParams label params) ++ "\n"

gvLabelAndParams :: String -> [(String,String)] -> String
gvLabelAndParams l p = 
  gvParams $ if null l then p else ("label", l) : p

gvParams :: [(String,String)] -> String
gvParams [] = ""
gvParams p  = "[ " ++ (concat $ intersperse ", " $ map showPair p) ++ " ]"
  where showPair (a,v) = a ++ "=\"" ++ v ++ "\""
\end{code}

\subsection{Some instances}

\begin{code}
instance (GraphvizShow f b) => GraphvizShow f (Maybe b) where
  graphvizShowAsSubgraph _ _ Nothing  = ""
  graphvizShowAsSubgraph f p (Just b) = graphvizShowAsSubgraph f p b 

  graphvizLabel _ Nothing  = ""
  graphvizLabel f (Just b) = graphvizLabel f b

  graphvizParams _ Nothing = [] 
  graphvizParams f (Just b) = graphvizParams f b
\end{code}

\fnlabel{gvShowTree} displays a tree in graphviz format.  Note that
we could make this an instance of GraphvizShow, but I'm not too sure
about the wisdom of such a move.  

Maybe if we had some really super-sophisticated types in Haskell, where
we can define this as the default instance which could be overrided by
something more specific, that would be cool.

The prefix argument is interpreted as the name of the top node.  Node
names below are basically Gorn addresses (e.g. n0x2x3 means 3rd child of
the 2nd child of the root) to keep them distinct.  Note : We use the
letter `x' as seperator because graphviz will choke on `.' or `-', even
underscore.

\begin{code}
gvShowTree :: 
  (GraphvizShowNode f n) => 
     (n->[(String,String)]) -- ^ function to convert a node to a list of graphviz parameters for the edge 
  -> f                      -- ^ GraphvizShow flag
  -> String                 -- ^ node prefix
  -> (Tree n)               -- ^ the tree
  -> String
gvShowTree edgeFn f prefix t = 
  "edge [ arrowhead = none ]\n" ++ gvShowTreeHelper edgeFn f prefix t  

gvShowTreeHelper :: (GraphvizShowNode f n) => (n->[(String,String)]) -> f -> String -> (Tree n) -> String
gvShowTreeHelper edgeFn f prefix (Node node l) = 
   let showNode = graphvizShowNode f prefix 
       showKid index kid = 
         gvShowTreeHelper edgeFn f kidname kid ++ " " 
         ++ (gvEdge prefix kidname "" (edgeFn node))
         where kidname = prefix ++ "x" ++ (show index)
   in showNode node ++ "\n" ++ (concat $ zipWith showKid [0..] l)
\end{code}


\section{Invocation}

Calls graphviz. The first argument is a String in graphviz's dot format.
The second is the name of the file graphviz should write the first argument 
to. The third is the name of output the file graphviz should write its graphic
to.  If the second argument is the empty string, then we don't write any 
dot file.

\begin{code}
graphviz:: String -> String -> String -> IO ExitCode
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
   when (not $ null dotFile) $ writeFile dotFile dot
   (_, toGV, _, pid) <- runInteractiveProcess "dot" dotArgs Nothing Nothing
   when (null dotFile) $ do 
     hPutStrLn toGV dot 
     hClose toGV
   waitForProcess pid
\end{code}


