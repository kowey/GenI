-- GenI surface realiser
-- Copyright (C) 2005-2009 Carlos Areces and Eric Kow
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverlappingInstances, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | This module provides basic datatypes specific to Tree Adjoining Grammar
--   tree schemata.
module NLP.GenI.TreeSchemata (
   Macros, emptyMacro,
   SchemaTree, SchemaNode, Ttree(..), Ptype(..),

   -- Functions from Tree GNode
   root, rootUpd, foot, setLexeme, setAnchor, lexemeAttributes,
   crushTreeGNode,

   -- GNode
   GNode(..), emptyGNode, gnnameIs, NodeName,
   GType(..), gCategory, showLexeme,
   crushGNode,
 ) where

import qualified Data.Map as Map
import Data.Binary
import Data.List (intersperse)
import Data.Tree
import Data.Text ( Text )

import Data.Generics (Data)
import Data.Typeable (Typeable)

import NLP.GenI.GeniVal ( GeniVal(..), DescendGeniVal(..), Collectable(..),
                        )
import NLP.GenI.FeatureStructures ( AvPair(..), Flist, crushFlist )
import NLP.GenI.Semantics ( Sem )

import NLP.GenI.General (filterTree, listRepNode, geniBug,)

import Control.DeepSeq

-- ----------------------------------------------------------------------
-- Tree schemata

-- In GenI, the tree schemata are called `macros' for historical reasons.
-- We are working to phase out this name in favour of the more standard
-- `tree schema(ta)'.
-- ----------------------------------------------------------------------

type SchemaTree = Ttree SchemaNode
type SchemaNode = GNode [GeniVal]
type Macros = [SchemaTree]

data Ttree a = TT
  { params  :: [GeniVal]
  , pfamily :: String
  , pidname :: String
  , pinterface :: Flist GeniVal
  , ptype :: Ptype
  , psemantics :: Maybe Sem
  , ptrace :: [String]
  , tree :: Tree a }
  deriving (Show, Data, Typeable, Eq)

data Ptype = Initial | Auxiliar | Unspecified
             deriving (Show, Eq, Data, Typeable)

instance DescendGeniVal v => DescendGeniVal (Ttree v) where
  descendGeniVal s mt =
    mt { params = descendGeniVal s (params mt)
       , tree   = descendGeniVal s (tree mt)
       , pinterface  = descendGeniVal s (pinterface mt)
       , psemantics = descendGeniVal s (psemantics mt) }

instance (Collectable a) => Collectable (Ttree a) where
  collect mt = (collect $ params mt) . (collect $ tree mt) .
               (collect $ psemantics mt) . (collect $ pinterface mt)

-- | A null tree which you can use for various debugging or display purposes.
emptyMacro :: SchemaTree
emptyMacro = TT { params  = [],
                  pidname = "",
                  pfamily = "",
                  pinterface = [],
                  ptype = Unspecified,
                  psemantics = Nothing,
                  ptrace = [],
                  tree  = Node emptyGNode []
                 }

-- ----------------------------------------------------------------------
-- Tree manipulation
-- ----------------------------------------------------------------------

-- Traversal

instance DescendGeniVal a => DescendGeniVal (Map.Map k a) where
  descendGeniVal s = {-# SCC "descendGeniVal" #-} Map.map (descendGeniVal s)

instance (Collectable a => Collectable (Tree a)) where
  collect = collect.flatten

-- Utility functions

root :: Tree a -> a
root (Node a _) = a

rootUpd :: Tree a -> a -> Tree a
rootUpd (Node _ l) b = (Node b l)

foot :: Tree (GNode a) -> GNode a
foot t = case filterTree (\n -> gtype n == Foot) t of
         [x] -> x
         _   -> geniBug $ "foot returned weird result"

-- | Given a lexical item @s@ and a Tree GNode t, returns the tree t'
--   where l has been assigned to the anchor node in t'
setAnchor :: [String] -> Tree (GNode a) -> Tree (GNode a)
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
setLexeme :: [String] -> Tree (GNode a) -> Tree (GNode a)
setLexeme l (Node a []) = Node a [ Node subanc [] ]
  where subanc = emptyGNode { gnname = '_' : ((gnname a) ++ ('.' : (concat l)))
                            , gaconstr = True
                            , glexeme = l}
setLexeme _ _ = geniBug "impossible case in setLexeme - subtree with kids"

-- ----------------------------------------------------------------------
-- TAG nodes (GNode)
-- ----------------------------------------------------------------------

-- | A single node of a TAG tree.
data GNode gv =
             GN{gnname :: NodeName,
                gup    :: Flist gv,   -- ^ top feature structure
                gdown  :: Flist gv,   -- ^ bottom feature structure
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

-- Traversal

instance Collectable gv => Collectable (GNode gv) where
  collect n = (collect $ gdown n) . (collect $ gup n)

instance DescendGeniVal v => DescendGeniVal (GNode v) where
  descendGeniVal s gn =
    gn { gup = descendGeniVal s (gup gn)
       , gdown = descendGeniVal s (gdown gn) }

-- Utilities

-- | A null 'GNode' which you can use for various debugging or display purposes.
emptyGNode :: GNode gv
emptyGNode = GN { gnname = "",
                  gup = [], gdown = [],
                  ganchor = False,
                  glexeme = [],
                  gtype = Other,
                  gaconstr = False,
                  gorigin = "" }

gnnameIs :: NodeName -> GNode gv -> Bool
gnnameIs n = (== n) . gnname

-- | Return the value of the "cat" attribute, if available
gCategory :: Flist GeniVal -> Maybe GeniVal
gCategory top =
  case [ v | AvPair "cat" v <- top ] of
  []  -> Nothing
  [c] -> Just c
  _   -> geniBug $ "Impossible case: node with more than one category"

-- | Attributes recognised as lexemes, in order of preference
lexemeAttributes :: [Text]
lexemeAttributes = [ "lex", "phon", "cat" ]

-- Pretty printing

-- | The default show for GNode tries to be very compact; it only shows the value
--   for cat attribute and any flags which are marked on that node.
instance Show (GNode GeniVal) where
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

-- Fancy disjunction

crushTreeGNode :: Tree (GNode [GeniVal]) -> Maybe (Tree (GNode GeniVal))
crushTreeGNode (Node x xs) =
 do x2  <- crushGNode x
    xs2 <- mapM crushTreeGNode xs
    return $ Node x2 xs2

crushGNode :: GNode [GeniVal] -> Maybe (GNode GeniVal)
crushGNode gn =
  do gup2   <- crushFlist (gup gn)
     gdown2 <- crushFlist (gdown gn)
     return $ GN { gnname = gnname gn
                 , gup = gup2
                 , gdown = gdown2
                 , ganchor = ganchor gn
                 , glexeme = glexeme gn
                 , gtype = gtype gn
                 , gaconstr = gaconstr gn
                 , gorigin = gorigin gn}


instance Binary Ptype where
  put Initial = putWord8 0
  put Auxiliar = putWord8 1
  put Unspecified = putWord8 2
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return Initial
      1 -> return Auxiliar
      2 -> return Unspecified
      _ -> fail "no parse"

instance Binary gv => Binary (GNode gv) where
  put (GN a b c d e f g h) = put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> get >>= \f -> get >>= \g -> get >>= \h -> return (GN a b c d e f g h)

instance Binary GType where
  put Subs = putWord8 0
  put Foot = putWord8 1
  put Lex = putWord8 2
  put Other = putWord8 3
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return Subs
      1 -> return Foot
      2 -> return Lex
      3 -> return Other
      _ -> fail "no parse"

instance (Binary a) => Binary (Ttree a) where
  put (TT a b c d e f g h) = put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> get >>= \f -> get >>= \g -> get >>= \h -> return (TT a b c d e f g h)

-- Node type used during parsing of the grammar
instance NFData GType where
  rnf x = x `seq` ()

instance NFData Ptype where
  rnf x = x `seq` ()

-- | A single node of a TAG tree.
instance NFData gv => NFData (GNode gv) where
  rnf (GN x1 x2 x3 x4 x5 x6 x7 x8)
          = rnf x1 `seq`
              rnf x2 `seq`
                rnf x3 `seq`
                  rnf x4 `seq`
                    rnf x5 `seq`
                      rnf x6 `seq`
                        rnf x7 `seq` rnf x8 `seq` ()
