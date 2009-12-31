{-# OPTIONS -fglasgow-exts #-}

{-
 GenI surface realiser
 Copyright (C) 2005 Carlos Areces and Eric Kow
 
 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
-}

{- | Graphviz is an open source tool which converts an abstract
     representation of a graph (node foo is connected to node bar, etc.)
     into a nicely laid out graphic.  This module contains methods 
     to invoke graphviz and to convert graphs and trees to its input format.

     You can download this (open source) tool at
     <http://www.research.att.com/sw/tools/graphviz>
-}

module NLP.GenI.Graphviz
where

import Control.Concurrent (forkIO)
import Control.Exception (bracket, evaluate)
import Control.Monad(when)
import Data.List(intersperse)
import Data.Tree
import System.IO ( hClose )
import System.IO.UTF8
import Prelude hiding ( writeFile )
import System.Exit(ExitCode)

import NLP.GenI.SysGeni(waitForProcess, runInteractiveProcess)

{- |
     Data structures which can be visualised with GraphViz should
     implement this class.  Note the first argument to graphvizShowGraph is
     so that you can parameterise your show function (i.e. pass in
     flags to change the way you show particular object).  Note
     that by default, all graphs are treated as directed graphs.  You
     can hide this by turning off edge arrows.
-}
class GraphvizShow flag b where
  graphvizShowGraph       :: flag -> b -> String
  graphvizShowAsSubgraph  :: flag   -- ^ flag
                          -> String -- ^ prefix
                          -> b      -- ^ item
                          -> String -- ^ gv output 
  graphvizLabel           :: flag   -- ^ flag
                          -> b      -- ^ item
                          -> String -- ^ gv output
  graphvizParams          :: flag -> b -> [String] 

  graphvizShowGraph f b  = 
    let l = graphvizLabel f b
    in "digraph {\n" 
       ++ (unlines $ graphvizParams f b)
       ++ graphvizShowAsSubgraph f "_" b ++ "\n"
       ++ (if null l then "" else " label = \"" ++ l ++ "\";\n")
       ++ "}"
  graphvizLabel _ _ = ""
  graphvizParams _ _ = []

class GraphvizShowNode flag b where
  graphvizShowNode :: flag   -- ^ flag 
                   -> String -- ^ prefix 
                   -> b      -- ^ item 
                   -> String -- ^ gv output

-- | Things which are meant to be displayed within some other graph
--   as (part) of a node label
class GraphvizShowString flag b where
  graphvizShow :: flag   -- ^ flag
               -> b      -- ^ item
               -> String -- ^ gv output

-- | Note: the 'dotFile' argument allows you to save the intermediary
-- dot output to a file.  You can pass in the empty string if you don't
toGraphviz :: (GraphvizShow f a) => f 
                                 -> a 
                                 -> String -- ^ the 'dotFile'
                                 -> String -> IO ExitCode 
toGraphviz p x dotFile outputFile = do
   graphviz (graphvizShowGraph p x) dotFile outputFile

-- ---------------------------------------------------------------------
-- useful utility functions
-- ---------------------------------------------------------------------

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

-- ---------------------------------------------------------------------
-- some instances 
-- ---------------------------------------------------------------------

instance (GraphvizShow f b) => GraphvizShow f (Maybe b) where
  graphvizShowAsSubgraph _ _ Nothing  = ""
  graphvizShowAsSubgraph f p (Just b) = graphvizShowAsSubgraph f p b 

  graphvizLabel _ Nothing  = ""
  graphvizLabel f (Just b) = graphvizLabel f b

  graphvizParams _ Nothing = [] 
  graphvizParams f (Just b) = graphvizParams f b

-- | Displays a tree in graphviz format.  
{- Note that we could make this an
   instance of GraphvizShow, but I'm not too sure about the wisdom of
   such a move.  

   Maybe if we had some really super-sophisticated types in Haskell, where
   we can define this as the default instance which could be overrided by
   something more specific, that would be cool.

   The prefix argument is interpreted as the name of the top node.  Node
   names below are basically Gorn addresses (e.g. n0x2x3 means 3rd child of
   the 2nd child of the root) to keep them distinct.  Note : We use the
   letter `x' as seperator because graphviz will choke on `.' or `-', even
   underscore. -}
gvShowTree :: (GraphvizShowNode f n) => 
     (n->[(String,String)]) -- ^ function to convert a node to a list of graphviz parameters for the edge 
  -> f                      -- ^ GraphvizShow flag
  -> String                 -- ^ node prefix
  -> (Tree n)               -- ^ the tree
  -> String
gvShowTree edgeFn f prefix t = 
  "edge [ arrowhead = none ]\n" ++ gvShowTreeHelper edgeFn f prefix t  

gvShowTreeHelper :: forall n . forall f . (GraphvizShowNode f n) => (n->[(String,String)]) -> f -> String -> (Tree n) -> String
gvShowTreeHelper edgeFn f prefix (Node node l) = 
   let showNode = graphvizShowNode f prefix 
       showKid :: Integer -> Tree n -> String
       showKid index kid = 
         gvShowTreeHelper edgeFn f kidname kid ++ " " 
         ++ (gvEdge prefix kidname "" (edgeFn node))
         where kidname = prefix ++ "x" ++ (show index)
   in showNode node ++ "\n" ++ (concat $ zipWith showKid [0..] l)

-- ---------------------------------------------------------------------
-- invocation 
-- ---------------------------------------------------------------------

-- | Calls graphviz. If the second argument is the empty string, then we
-- just send stuff directly to dot's stdin

graphviz :: String -- ^ graphviz's dot format.
         -> String -- ^ the name of the file graphviz should write the dot 
         -> String -- ^ the name of the file graphviz should write its output 
         -> IO ExitCode

-- We write the dot String to a temporary file which we then feed to graphviz.
-- This is avoid complications with fork and pipes.  We use png output even
-- though it's uglier, because we don't have a wxhaskell widget that can 
-- display postscript... do we?

graphviz dot dotFile outputFile = do
   let dotArgs' = ["-Gfontname=courier", 
                   "-Nfontname=courier", 
                   "-Efontname=courier", 
                   "-Gcharset=utf-8",
                   "-Tpng", "-o" ++ outputFile ]
       dotArgs = dotArgs' ++ (if (null dotFile) then [] else [dotFile])
   -- putStrLn ("sending to graphviz:\n" ++ dot) 
   when (not $ null dotFile) $ writeFile dotFile dot
   bracket
    (runInteractiveProcess "dot" dotArgs Nothing Nothing)
    (\(inh,outh,errh,_) -> hClose inh >> hClose outh >> hClose errh)
    $ \(inh,outh,errh,pid) -> do
       when (null dotFile) $ hPutStrLn inh dot 
       hClose inh
       -- see http://www.haskell.org/pipermail/haskell-cafe/2008-May/042994.html
       -- wait for all the output
       output <- hGetContents outh
       evaluate (length output)
       -- fork off a thread to pull on the stderr
       -- so if the process writes to stderr we do not block.
       -- NB. do the hGetContents synchronously, otherwise the outer
       -- bracket can exit before this thread has run, and hGetContents
       -- will fail.
       err <- hGetContents errh
       forkIO $ do evaluate (length err); return ()
       waitForProcess pid
