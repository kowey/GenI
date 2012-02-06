{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

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
import Data.GraphViz.Attributes.Complete

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Text.Lazy ( Text )
import Data.Tree
import Prelude hiding ( writeFile )

{- |
     Data structures which can be visualised with GraphViz should
     implement this class.  Note the first argument to graphvizShowGraph is
     so that you can parameterise your show function (i.e. pass in
     flags to change the way you show particular object).  Note
     that by default, all graphs are treated as directed graphs.  You
     can hide this by turning off edge arrows.
-}
class GraphvizShow b where
  graphvizShowGraph       :: b -> DotGraph Text
  graphvizShowAsSubgraph  :: Text   -- ^ prefix
                          -> b      -- ^ item
                          -> [DotSubGraph Text] -- ^ gv output
  graphvizLabel           :: b      -- ^ item
                          -> Text   -- ^ gv output
  graphvizParams          :: b -> [GlobalAttributes]

  graphvizShowGraph b  =
    DotGraph False True Nothing $ DotStmts
       (addLabel (graphvizLabel b) $ graphvizParams b)
       (graphvizShowAsSubgraph "_" b)
       []
       []
    where
      addLabel :: Text -> [GlobalAttributes] -> [GlobalAttributes]
      addLabel "" = id
      addLabel l  = (GraphAttrs [Label (StrLabel l)] :)

  graphvizLabel _ = ""
  graphvizParams _ = []

class GraphvizShowNode b where
  graphvizShowNode :: Text   -- ^ prefix
                   -> b      -- ^ item 
                   -> DotNode Text -- ^ gv output

-- | Things which are meant to be displayed within some other graph
--   as (part) of a node label
class GraphvizShowString b where
  graphvizShow :: b      -- ^ item
               -> Text   -- ^ gv output

-- | Note: the 'dotFile' argument allows you to save the intermediary
-- dot output to a file.  You can pass in the empty string if you don't
toGraphviz :: GraphvizShow a     => a
                                 -> String -- ^ the 'dotFile'
                                 -> String -> IO FilePath
toGraphviz x dotFile outputFile = do
   T.writeFile dotFile (printIt g)
   runGraphviz g Png outputFile
 where
  g = graphvizShowGraph x

-- ---------------------------------------------------------------------
-- useful utility functions
-- ---------------------------------------------------------------------

gvUnlines :: [Text] -> Text
gvUnlines = T.intercalate "\n"

-- ---------------------------------------------------------------------
-- some instances 
-- ---------------------------------------------------------------------

instance GraphvizShow b => GraphvizShow (Maybe b) where
  graphvizShowAsSubgraph _ Nothing  = []
  graphvizShowAsSubgraph p (Just b) = graphvizShowAsSubgraph p b

  graphvizLabel Nothing  = ""
  graphvizLabel (Just b) = graphvizLabel b

  graphvizParams Nothing = []
  graphvizParams (Just b) = graphvizParams b

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
gvShowTree :: GraphvizShowNode n =>
     Text                   -- ^ node prefix
  -> Tree n                 -- ^ the tree
  -> DotSubGraph Text
gvShowTree prefix t =
  DotSG False Nothing $ DotStmts atts [] nodes edges
 where
  atts = [ EdgeAttrs [ ArrowHead noArrow ] ] -- should be none
  (nodes, edges) = second concat . unzip $ walk prefix t
  --
  walk pref (Node node xs) =
    let mkPrefix i = T.concat [pref, "x", T.pack (show i)]
        ns = graphvizShowNode pref node
        ps = map mkPrefix [1::Int .. length xs]
        es = map (\n -> DotEdge pref n []) ps
    in (ns, es) : concat (zipWith walk ps xs)
