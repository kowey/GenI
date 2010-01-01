% GenI surface realiser
% Copyright (C) 2005-2009 Carlos Areces and Eric Kow
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

\chapter{Tree schemata}
\label{cha:TreeSchemata}

This module provides basic datatypes specific to Tree Adjoining Grammar
tree schemata.

\ignore{
\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module NLP.GenI.TreeSchemata (
   Macros, emptyMacro,
   MTtree, Ttree(..), Ptype(..),

   -- Functions from Tree GNode
   root, rootUpd, foot, setLexeme, setAnchor, lexemeAttributes,

   -- GNode
   GNode(..), emptyGNode, gnnameIs, NodeName,
   GType(..), gCategory, showLexeme,
 ) where

import qualified Data.Map as Map
import Data.List (intersperse)
import Data.Tree

import Data.Generics (Data)
import Data.Typeable (Typeable)

import NLP.GenI.GeniVal ( GeniVal(..), DescendGeniVal(..), Collectable(..),
                        )
import NLP.GenI.FeatureStructures ( AvPair(..), Flist )
import NLP.GenI.Semantics ( Sem )

import NLP.GenI.General (filterTree, listRepNode, geniBug,)
\end{code}
}

% ----------------------------------------------------------------------
\section{Tree schemata}
% ----------------------------------------------------------------------

In GenI, the tree schemata are called `macros' for historical reasons.
We are working to phase out this name in favour of the more standard
`tree schema(ta)'.

\begin{code}
type MTtree = Ttree GNode
type Macros = [MTtree]

data Ttree a = TT
  { params  :: [GeniVal]
  , pfamily :: String
  , pidname :: String
  , pinterface :: Flist GeniVal
  , ptype :: Ptype
  , psemantics :: Maybe Sem
  , ptrace :: [String]
  , tree :: Tree a }
  deriving (Show, Data, Typeable)

data Ptype = Initial | Auxiliar | Unspecified
             deriving (Show, Eq, Data, Typeable)

instance DescendGeniVal (Ttree GNode) where
  descendGeniVal s mt =
    mt { params = descendGeniVal s (params mt)
       , tree   = descendGeniVal s (tree mt)
       , pinterface  = descendGeniVal s (pinterface mt)
       , psemantics = descendGeniVal s (psemantics mt) }

instance (Collectable a) => Collectable (Ttree a) where
  collect mt = (collect $ params mt) . (collect $ tree mt) .
               (collect $ psemantics mt) . (collect $ pinterface mt)

-- | A null tree which you can use for various debugging or display purposes.
emptyMacro :: MTtree
emptyMacro = TT { params  = [],
                  pidname = "",
                  pfamily = "",
                  pinterface = [],
                  ptype = Unspecified,
                  psemantics = Nothing,
                  ptrace = [],
                  tree  = Node emptyGNode []
                 }
\end{code}

% ----------------------------------------------------------------------
\section{Tree manipulation}
% ----------------------------------------------------------------------

\subsection{Traversal}

\begin{code}
instance DescendGeniVal a => DescendGeniVal (Map.Map k a) where
  descendGeniVal s = {-# SCC "descendGeniVal" #-} Map.map (descendGeniVal s)

instance (Collectable a => Collectable (Tree a)) where
  collect = collect.flatten
\end{code}

\subsection{Utility functions}

\begin{code}
root :: Tree a -> a
root (Node a _) = a

rootUpd :: Tree a -> a -> Tree a
rootUpd (Node _ l) b = (Node b l)

foot :: Tree GNode -> GNode
foot t = case filterTree (\n -> gtype n == Foot) t of
         [x] -> x
         _   -> geniBug $ "foot returned weird result"

-- | Given a lexical item @s@ and a Tree GNode t, returns the tree t'
--   where l has been assigned to the anchor node in t'
setAnchor :: [String] -> Tree GNode -> Tree GNode
setAnchor s t =
  let filt (Node a []) = (gtype a == Lex && ganchor a)
      filt _ = False
  in case listRepNode (setLexeme s) filt [t] of
     ([r],True) -> r
     _ -> geniBug $ "setLexeme " ++ show s ++ " returned weird result"

-- | Given a lexical item @l@ and a tree node @n@ (actually a subtree
--   with no children), return the same node with the lexical item as
--   its unique child.  The idea is that it converts terminal lexeme nodes
--   into preterminal nodes where the actual terminal is the given lexical
--   item
setLexeme :: [String] -> Tree GNode -> Tree GNode
setLexeme l (Node a []) = Node a [ Node subanc [] ]
  where subanc = emptyGNode { gnname = '_' : ((gnname a) ++ ('.' : (concat l)))
                            , gaconstr = True
                            , glexeme = l}
setLexeme _ _ = geniBug "impossible case in setLexeme - subtree with kids"
\end{code}

% ----------------------------------------------------------------------
\section{TAG nodes (GNode)}
% ----------------------------------------------------------------------

\begin{code}
-- | A single node of a TAG tree.
data GNode = GN{gnname :: NodeName,
                gup    :: Flist GeniVal,      -- ^ top feature structure
                gdown  :: Flist GeniVal,      -- ^ bottom feature structure
                ganchor  :: Bool,     -- ^ @False@ for na nodes
                glexeme  :: [String], -- ^ @[]@ for na nodes
                gtype    :: GType,
                gaconstr :: Bool,
                gorigin  :: String  -- ^ for TAG, this would be the elementary tree
                                    --   that this node originally came from
                }
           deriving (Eq, Data, Typeable)

-- Node type used during parsing of the grammar
data GType = Subs | Foot | Lex | Other
           deriving (Show, Eq, Data, Typeable)

type NodeName = String
\end{code}

\subsection{Traversal}

\begin{code}
instance Collectable GNode where
  collect n = (collect $ gdown n) . (collect $ gup n)

instance DescendGeniVal GNode where
  descendGeniVal s gn =
    gn { gup = descendGeniVal s (gup gn)
       , gdown = descendGeniVal s (gdown gn) }
\end{code}

\subsection{Utilities}

\begin{code}
-- | A null 'GNode' which you can use for various debugging or display purposes.
emptyGNode :: GNode
emptyGNode = GN { gnname = "",
                  gup = [], gdown = [],
                  ganchor = False,
                  glexeme = [],
                  gtype = Other,
                  gaconstr = False,
                  gorigin = "" }

gnnameIs :: NodeName -> GNode -> Bool
gnnameIs n = (== n) . gnname
\end{code}

A TAG node may have a category.  In the core GenI algorithm, there is nothing
which distinguishes the category from any other attributes.  But for some
other uses, such as checking if it is a result or for display purposes, we
do treat this attribute differently.  We take here the convention that the
category of a node is associated to the attribute ``cat''.
\begin{code}
-- | Return the value of the "cat" attribute, if available
gCategory :: Flist GeniVal -> Maybe GeniVal
gCategory top =
  case [ v | AvPair "cat" v <- top ] of
  []  -> Nothing
  [c] -> Just c
  _   -> geniBug $ "Impossible case: node with more than one category"
\end{code}

A TAG node might also have a lexeme.  If we are lucky, this is explicitly
set in the glexeme field of the node.  Otherwise, we try to guess it from
a list of distinguished attributes (in order of preference).
\begin{code}
-- | Attributes recognised as lexemes, in order of preference
lexemeAttributes :: [String]
lexemeAttributes = [ "lex", "phon", "cat" ]
\end{code}

\subsection{Pretty printing}

The default show for GNode tries to be very compact; it only shows the value
for cat attribute and any flags which are marked on that node.

\begin{code}
instance Show GNode where
  show gn =
    let cat_ = case gCategory.gup $ gn of
               Nothing -> []
               Just c  -> show c
        lex_ = showLexeme $ glexeme gn
        --
        stub = concat $ intersperse ":" $ filter (not.null) [ cat_, lex_ ]
        extra = case (gtype gn) of
                   Subs -> " !"
                   Foot -> " *"
                   _    -> if (gaconstr gn)  then " #"   else ""
    in stub ++ extra

-- FIXME: will have to think of nicer way - one which involves
-- unpacking the trees :-(
showLexeme :: [String] -> String
showLexeme []   = ""
showLexeme [l]  = l
showLexeme xs   = concat $ intersperse "|" xs
\end{code}
