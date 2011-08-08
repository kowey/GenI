{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
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

import Control.Arrow ( second )
import Data.GraphViz
import Data.GraphViz.Printing ( printIt )

import Data.List(intercalate)
import Data.Tree
import System.IO.UTF8
import Prelude hiding ( writeFile )

{- |
     Data structures which can be visualised with GraphViz should
     implement this class.  Note the first argument to graphvizShowGraph is
     so that you can parameterise your show function (i.e. pass in
     flags to change the way you show particular object).  Note
     that by default, all graphs are treated as directed graphs.  You
     can hide this by turning off edge arrows.
-}
class GraphvizShow flag b where
  graphvizShowGraph       :: flag -> b -> DotGraph String
  graphvizShowAsSubgraph  :: flag   -- ^ flag
                          -> String -- ^ prefix
                          -> b      -- ^ item
                          -> [DotSubGraph String] -- ^ gv output
  graphvizLabel           :: flag   -- ^ flag
                          -> b      -- ^ item
                          -> String -- ^ gv output
  graphvizParams          :: flag -> b -> [GlobalAttributes]

  graphvizShowGraph f b  =
    DotGraph False True Nothing $ DotStmts
       (addLabel (graphvizLabel f b) $ graphvizParams f b)
       (graphvizShowAsSubgraph f "_" b)
       []
       []
    where
      addLabel "" = id
      addLabel l  = (GraphAttrs [Label (StrLabel l)] :)

  graphvizLabel _ _ = ""
  graphvizParams _ _ = []

class GraphvizShowNode flag b where
  graphvizShowNode :: flag   -- ^ flag 
                   -> String -- ^ prefix 
                   -> b      -- ^ item 
                   -> DotNode String -- ^ gv output

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
                                 -> String -> IO (Either String FilePath)
toGraphviz p x dotFile outputFile = do
   writeFile dotFile (printIt g)
   runGraphviz g Png outputFile
 where
  g = graphvizShowGraph p x

-- ---------------------------------------------------------------------
-- useful utility functions
-- ---------------------------------------------------------------------

gvUnlines :: [String] -> String
gvUnlines = intercalate "\n"

-- ---------------------------------------------------------------------
-- some instances 
-- ---------------------------------------------------------------------

instance (GraphvizShow f b) => GraphvizShow f (Maybe b) where
  graphvizShowAsSubgraph _ _ Nothing  = []
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
     f                      -- ^ GraphvizShow flag
  -> String                 -- ^ node prefix
  -> Tree n                 -- ^ the tree
  -> DotSubGraph String
gvShowTree f prefix t =
  DotSG False Nothing $ DotStmts atts [] nodes edges
 where
  atts = [ EdgeAttrs [ ArrowHead noArrow ] ] -- should be none
  (nodes, edges) = second concat . unzip $ walk prefix t
  --
  walk pref (Node node xs) =
    let ns = graphvizShowNode f pref node
        ps = zipWith (\i _ -> pref ++ "x" ++ show i) [0::Int ..] xs
        es = map (\n -> DotEdge pref n True []) ps
    in (ns, es) : concat (zipWith walk ps xs)
