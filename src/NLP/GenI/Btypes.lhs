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

\chapter{Btypes}
\label{cha:Btypes}

This module provides basic datatypes like GNode, as well as operations on trees
and nodes.

For now, it doubles as a catch-all core module by exposing functions from other
modules such as NLP.GenI.Semantics.

\ignore{
\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
module NLP.GenI.Btypes(
   -- Datatypes
   GNode(..), GType(Subs, Foot, Lex, Other), NodeName,
   Ttree(..), MTtree, SemPols, TestCase(..),
   Ptype(Initial,Auxiliar,Unspecified),
   Pred, Flist, AvPair(..), GeniVal(..),
   Lexicon, ILexEntry(..), MorphLexEntry, Macros, Sem, LitConstr, SemInput, Subst,
   emptyLE, emptyGNode, emptyMacro,

   -- GNode stuff
   gCategory, showLexeme, lexemeAttributes, gnnameIs,

   -- Functions from Tree GNode
   plugTree, spliceTree,
   root, rootUpd, foot, setLexeme, setAnchor,

   -- Functions from Sem
   toKeys, subsumeSem, sortSem, showSem, showPred,
   emptyPred,

   -- Functions from Flist
   sortFlist, unify, unifyFeat, mergeSubst,
   showFlist, showPairs, showAv,

   -- Other functions
   replace, DescendGeniVal(..), replaceList,
   Collectable(..), Idable(..),
   alphaConvert, alphaConvertById,
   fromGConst, fromGVar,
   isConst, isVar, isAnon,

   -- Polarities

) where


-- import Debug.Trace -- for test stuff
import Data.List
import Data.Maybe ( mapMaybe )
import Data.Generics (Data)
import Data.Typeable (Typeable)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tree

import Data.Generics.PlateDirect

import NLP.GenI.FeatureStructures
import NLP.GenI.General(filterTree, listRepNode, geniBug)
import NLP.GenI.GeniVal
import NLP.GenI.Semantics

--instance Show (IO()) where
--  show _ = ""
\end{code}
}

% ----------------------------------------------------------------------
\section{Grammar}
% ----------------------------------------------------------------------

A grammar is composed of some unanchored trees (macros) and individual
lexical entries. The trees are grouped into families. Every lexical
entry is associated with a single family.  See section section
\ref{sec:combine_macros} for the process that combines lexical items
and trees into a set of anchored trees.

\begin{code}
type MTtree = Ttree GNode
type Macros = [MTtree]

data Ttree a = TT
  { params  :: [GeniVal]
  , pfamily :: String
  , pidname :: String
  , pinterface :: Flist
  , ptype :: Ptype
  , psemantics :: Maybe Sem
  , ptrace :: [String]
  , tree :: Tree a }
  deriving (Show, Data, Typeable)

data Ptype = Initial | Auxiliar | Unspecified
             deriving (Show, Eq, Data, Typeable)

instance Biplate (Ttree String) GeniVal where
  biplate (TT zps x1 x2 zint x3 zsem x4 x5) =
     plate TT ||* zps  |- x1 |- x2
              ||+ zint |- x3
              |+ zsem |- x4 |- x5

instance Biplate (Ttree GNode) GeniVal where
  biplate (TT zps x1 x2 zint x3 zsem x4 zt) =
     plate TT ||* zps  |- x1 |- x2
              ||+ zint |- x3
              |+ zsem |- x4
              |+ zt

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

\paragraph{Lexical entries}

\begin{code}
-- | A lexicon maps semantic predicates to lexical entries.
type Lexicon = Map.Map String [ILexEntry]
type SemPols  = [Int]
data ILexEntry = ILE
    { -- normally just a singleton, useful for merging synonyms
      iword       :: [String]
    , ifamname    :: String
    , iparams     :: [GeniVal]
    , iinterface  :: Flist
    , ifilters    :: Flist
    , iequations  :: Flist
    , iptype      :: Ptype
    , isemantics  :: Sem
    , isempols    :: [SemPols] }
  deriving (Show, Eq, Data, Typeable)

instance Biplate ILexEntry GeniVal where
  biplate (ILE x1 x2 zps zint zfilts zeq x3 zsem x4) =
    plate ILE |- x1 |- x2
              ||* zps
              ||+ zint
              ||+ zfilts
              ||+ zeq  |- x3
              ||+ zsem |- x4

instance DescendGeniVal ILexEntry where
  descendGeniVal s i =
    i { iinterface  = descendGeniVal s (iinterface i)
      , iequations  = descendGeniVal s (iequations i)
      , isemantics  = descendGeniVal s (isemantics i)
      , iparams = descendGeniVal s (iparams i) }

instance Collectable ILexEntry where
  collect l = (collect $ iinterface l) . (collect $ iparams l) .
              (collect $ ifilters l) . (collect $ iequations l) .
              (collect $ isemantics l)

emptyLE :: ILexEntry
emptyLE = ILE { iword = [],
                ifamname = "",
                iparams = [],
                iinterface   = [],
                ifilters = [],
                iptype = Unspecified,
                isemantics = [],
                iequations = [],
                isempols   = [] }
\end{code}

\begin{code}
type MorphLexEntry = (String,String,Flist)
\end{code}

% ----------------------------------------------------------------------
\section{TAG nodes (GNode)}
% ----------------------------------------------------------------------

\begin{code}
-- | A single node of a TAG tree.
data GNode = GN{gnname :: NodeName,
                gup    :: Flist,      -- ^ top feature structure
                gdown  :: Flist,      -- ^ bottom feature structure
                ganchor  :: Bool,     -- ^ @False@ for na nodes
                glexeme  :: [String], -- ^ @[]@ for na nodes
                gtype    :: GType,
                gaconstr :: Bool,
                gorigin  :: String  -- ^ for TAG, this would be the elementary tree
                                    --   that this node originally came from
                }
           deriving (Eq, Data, Typeable)

instance Biplate GNode GeniVal where
  biplate (GN x1 zu zd x2 x3 x4 x5 x6) =
     plate GN |- x1
              ||+ zu
              ||+ zd |- x2 |- x3 |- x4 |- x5 |- x6

instance Biplate (Tree GNode) GeniVal where
  biplate (Node zn zkids) = plate Node |+ zn ||+ zkids

-- Node type used during parsing of the grammar
data GType = Subs | Foot | Lex | Other
           deriving (Show, Eq, Data, Typeable)

type NodeName = String

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
gCategory :: Flist -> Maybe GeniVal
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

\paragraph{show (GNode)} the default show for GNode tries to
be very compact; it only shows the value for cat attribute
and any flags which are marked on that node.

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

A Replacement on a GNode consists of replacements on its top and bottom
feature structures

\begin{code}
instance DescendGeniVal GNode where
  descendGeniVal s gn =
    gn { gup = descendGeniVal s (gup gn)
       , gdown = descendGeniVal s (gdown gn) }
\end{code}

% ----------------------------------------------------------------------
\section{Tree manipulation}
% ----------------------------------------------------------------------

\begin{code}
root :: Tree a -> a
root (Node a _) = a

rootUpd :: Tree a -> a -> Tree a
rootUpd (Node _ l) b = (Node b l)

foot :: Tree GNode -> GNode
foot t = case filterTree (\n -> gtype n == Foot) t of
         [x] -> x
         _   -> geniBug $ "foot returned weird result"
\end{code}

\begin{code}
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

\subsection{Substitution and Adjunction}

This module handles just the tree-cutting aspects of TAG substitution and
adjunction.  We do substitution with a very general \fnreflite{plugTree}
function, whose only job is to plug two trees together at a specified node.
Note that this function is also used to implement adjunction.

\begin{code}
-- | Plug the first tree into the second tree at the specified node.
--   Anything below the second node is silently discarded.
--   We assume the trees are pluggable; it is treated as a bug if
--   they are not!
plugTree :: Tree NodeName -> NodeName -> Tree NodeName -> Tree NodeName
plugTree male n female =
  case listRepNode (const male) (nmatch n) [female] of
  ([r], True) -> r
  _           -> geniBug $ "unexpected plug failure at node " ++ n

-- | Given two trees 'auxt' and 't', splice the tree 'auxt' into
--   't' via the TAG adjunction rule.
spliceTree :: NodeName      -- ^ foot node of the aux tree
           -> Tree NodeName -- ^ aux tree
           -> NodeName      -- ^ place to adjoin in target tree
           -> Tree NodeName -- ^ target tree
           -> Tree NodeName
spliceTree f auxT n targetT =
  case findSubTree n targetT of -- excise the subtree at n
  Nothing -> geniBug $ "Unexpected adjunction failure. " ++
                       "Could not find node " ++ n ++ " of target tree."
  Just eT -> -- plug the excised bit into the aux
             let auxPlus = plugTree eT f auxT
             -- plug the augmented aux at n
             in  plugTree auxPlus n targetT

nmatch :: NodeName -> Tree NodeName -> Bool
nmatch n (Node a _) = a == n

findSubTree :: NodeName -> Tree NodeName -> Maybe (Tree NodeName)
findSubTree n n2@(Node x ks)
  | x == n    = Just n2
  | otherwise = case mapMaybe (findSubTree n) ks of
                []    -> Nothing
                (h:_) -> Just h
\end{code}

% ----------------------------------------------------------------------
% ----------------------------------------------------------------------

\begin{code}
instance (Collectable a => Collectable (Tree a)) where
  collect = collect.flatten

instance Collectable GNode where
  collect n = (collect $ gdown n) . (collect $ gup n)
\end{code}

\subsection{DescendGeniVal}
\label{sec:replacable}
\label{sec:replacements}

The idea of replacing one variable value with another is something that
appears all over the place in GenI.  So we try to smooth out its use by
making a type class out of it.

\begin{code}

\end{code}

Substitution on list consists of performing substitution on
each item.  Each item, is independent of the other,
of course.

\begin{code}
instance DescendGeniVal a => DescendGeniVal (Map.Map k a) where
  descendGeniVal s = {-# SCC "descendGeniVal" #-} Map.map (descendGeniVal s)
\end{code}
\begin{code}
data TestCase = TestCase
       { tcName :: String
       , tcSemString :: String -- ^ for gui
       , tcSem  :: SemInput
       , tcExpected :: [String] -- ^ expected results (for testing)
       , tcOutputs :: [(String, Map.Map (String,String) [String])]
       -- ^ results we actually got, and their traces (for testing)
       } deriving Show
\end{code}


