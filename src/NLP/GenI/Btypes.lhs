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

This module provides basic datatypes like GNode, as well as operations
on trees, nodes and semantics.  Things here are meant to be relatively
low-level and primitive (well, with the exception of feature structure
unification, that is).

\ignore{
\begin{code}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
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

   -- Tests
   prop_unify_anon, prop_unify_self, prop_unify_sym
) where


-- import Debug.Trace -- for test stuff
import Data.List
import Data.Maybe ( mapMaybe )
import Data.Function ( on )
import Data.Generics (Data)
import Data.Typeable (Typeable)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tree

import Data.Generics.Biplate
import Data.Generics.PlateDirect

import NLP.GenI.General(filterTree, listRepNode, snd3, geniBug)
import NLP.GenI.GeniVal

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
\section{Features and variables}
% ----------------------------------------------------------------------

\begin{code}
type Flist   = [AvPair]
data AvPair  = AvPair { avAtt :: String
                      , avVal ::  GeniVal }
  deriving (Ord, Eq, Data, Typeable)

instance Biplate AvPair GeniVal where
  biplate (AvPair a v) = plate AvPair |- a |* v
\end{code}

\subsection{Collectable}

A Collectable is something which can return its variables as a set.
By variables, what I most had in mind was the GVar values in a
GeniVal.  This notion is probably not very useful outside the context of
alpha-conversion task, but it seems general enough that I'll keep it
around for a good bit, until either some use for it creeps up, or I find
a more general notion that I can transform this into.

\begin{code}
class Collectable a where
  collect :: a -> Set.Set String -> Set.Set String

instance Collectable a => Collectable (Maybe a) where
  collect Nothing  s = s
  collect (Just x) s = collect x s

instance (Collectable a => Collectable [a]) where
  collect l s = foldr collect s l

instance (Collectable a => Collectable (Tree a)) where
  collect = collect.flatten

-- Pred is what I had in mind here
instance ((Collectable a, Collectable b, Collectable c)
           => Collectable (a,b,c)) where
  collect (a,b,c) = collect a . collect b . collect c

instance Collectable GeniVal where
  collect (GVar v) s = Set.insert v s
  collect _ s = s

instance Collectable AvPair where
  collect (AvPair _ b) = collect b

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

instance DescendGeniVal AvPair where
  descendGeniVal s (AvPair a v) = {-# SCC "descendGeniVal" #-} AvPair a (descendGeniVal s v)

instance DescendGeniVal a => DescendGeniVal (String, a) where
  descendGeniVal s (n,v) = {-# SCC "descendGeniVal" #-} (n,descendGeniVal s v)

instance DescendGeniVal ([String], Flist) where
  descendGeniVal s (a,v) = {-# SCC "descendGeniVal" #-} (a, descendGeniVal s v)
\end{code}

\subsection{Idable}

\begin{code}
-- | An Idable is something that can be mapped to a unique id.
--   You might consider using this to implement Ord, but I won't.
--   Note that the only use I have for this so far (20 dec 2005)
--  is in alpha-conversion.
class Idable a where
  idOf :: a -> Integer
\end{code}

\subsection{Other feature and variable stuff}

Our approach to $\alpha$-conversion works by appending a unique suffix
to all variables in an object.  See section \ref{sec:fs_unification} for
why we want this.

\begin{code}
alphaConvertById :: (Collectable a, DescendGeniVal a, Idable a) => a -> a
alphaConvertById x = {-# SCC "alphaConvertById" #-}
  alphaConvert ('-' : (show . idOf $ x)) x

alphaConvert :: (Collectable a, DescendGeniVal a) => String -> a -> a
alphaConvert suffix x = {-# SCC "alphaConvert" #-}
  let vars   = Set.elems $ collect x Set.empty
      convert v = GVar (v ++ suffix)
      subst = Map.fromList $ map (\v -> (v, convert v)) vars
  in replace subst x
\end{code}

\begin{code}
-- | Sort an Flist according with its attributes
sortFlist :: Flist -> Flist
sortFlist = sortBy (compare `on` avAtt)

showFlist :: Flist -> String
showFlist f = "[" ++ showPairs f ++ "]"

showPairs :: Flist -> String
showPairs = unwords . map showAv

showAv :: AvPair -> String
showAv (AvPair y z) = y ++ ":" ++ show z

instance Show AvPair where
  show = showAv
\end{code}

% ----------------------------------------------------------------------
\section{Semantics}
\label{btypes_semantics}
% ----------------------------------------------------------------------

\begin{code}
-- handle, predicate, parameters
type Pred = (GeniVal, GeniVal, [GeniVal])
type Sem = [Pred]
type LitConstr = (Pred, [String])
type SemInput  = (Sem,Flist,[LitConstr])

instance Biplate Pred GeniVal where
  biplate (g1, g2, g3) = plate (,,) |* g1 |* g2 ||* g3

instance Biplate (Maybe Sem) GeniVal where
  biplate (Just s) = plate Just ||+ s
  biplate Nothing  = plate Nothing

data TestCase = TestCase
       { tcName :: String
       , tcSemString :: String -- ^ for gui
       , tcSem  :: SemInput
       , tcExpected :: [String] -- ^ expected results (for testing)
       , tcOutputs :: [(String, Map.Map (String,String) [String])]
       -- ^ results we actually got, and their traces (for testing)
       } deriving Show

emptyPred :: Pred
emptyPred = (GAnon,GAnon,[])
\end{code}

A replacement on a predicate is just a replacement on its parameters

\begin{code}
instance DescendGeniVal Pred where
  descendGeniVal s (h, n, lp) = (descendGeniVal s h, descendGeniVal s n, descendGeniVal s lp)
\end{code}

\begin{code}
showSem :: Sem -> String
showSem l =
    "[" ++ (unwords $ map showPred l) ++ "]"

showPred :: Pred -> String
showPred (h, p, l) = showh ++ show p ++ "(" ++ unwords (map show l) ++ ")"
  where
    hideh (GConst [x]) = "genihandle" `isPrefixOf` x
    hideh _ = False
    --
    showh = if (hideh h) then "" else (show h) ++ ":"
\end{code}

\begin{code}
-- | Given a Semantics, return the string with the proper keys
--   (propsymbol+arity) to access the agenda
toKeys :: Sem -> [String]
toKeys l = map (\(_,prop,par) -> show prop ++ (show $ length par)) l
\end{code}

\subsection{Semantic subsumption}
\label{fn:subsumeSem}

FIXME: comment fix

Given tsem the input semantics, and lsem the semantics of a potential
lexical candidate, returns a list of possible ways that the lexical
semantics could subsume the input semantics.  We return a pair with
the semantics that would result from unification\footnote{We need to
do this because there may be anonymous variables}, and the
substitutions that need to be propagated throughout the rest of the
lexical item later on.

Note: we return more than one possible substitution because s could be
different subsets of ts.  Consider, for example, \semexpr{love(j,m),
  name(j,john), name(m,mary)} and the candidate \semexpr{name(X,Y)}.

TODO WE ASSUME BOTH SEMANTICS ARE ORDERED and that the input semantics is
non-empty.

\begin{code}
subsumeSem :: Sem -> Sem -> [(Sem,Subst)]
subsumeSem tsem lsem =
  subsumeSemHelper ([],Map.empty) (reverse tsem) (reverse lsem)
\end{code}

This is tricky because each substep returns multiple results.  We solicit
the help of accumulators to keep things from getting confused.

\begin{code}
subsumeSemHelper :: (Sem,Subst) -> Sem -> Sem -> [(Sem,Subst)]
subsumeSemHelper _ [] _  =
  error "input semantics is non-empty in subsumeSemHelper"
subsumeSemHelper acc _ []      = [acc]
subsumeSemHelper acc tsem (hd:tl) =
  let (accSem,accSub) = acc
      -- does the literal hd subsume the input semantics?
      pRes = subsumePred tsem hd
      -- toPred reconstructs the literal hd with new parameters p.
      -- The head of the list is taken to be the handle.
      toPred p = (head p, snd3 hd, tail p)
      -- next adds a result from predication subsumption to
      -- the accumulators and goes to the next recursive step
      next (p,s) = subsumeSemHelper acc2 tsem2 tl2
         where tl2   = replace s tl
               tsem2 = replace s tsem
               acc2  = (toPred p : accSem, mergeSubst accSub s)
  in concatMap next pRes
\end{code}

\fnlabel{subsumePred}
The first Sem s1 and second Sem s2 are the same when we start we circle on s2
looking for a match for Pred, and meanwhile we apply the partical substitutions
to s1.  Note: we treat the handle as if it were a parameter.

\begin{code}
subsumePred :: Sem -> Pred -> [([GeniVal],Subst)]
subsumePred [] _ = []
subsumePred ((h1, p1, la1):l) (pred2@(h2,p2,la2)) =
    -- if we found the proper predicate
    if ((p1 == p2) && (length la1 == length la2))
    then let mrs  = unify (h1:la1) (h2:la2)
             next = subsumePred l pred2
         in maybe next (:next) mrs
    else if (p1 < p2) -- note that the semantics have to be reversed!
         then []
         else subsumePred l pred2
\end{code}

\subsection{Other semantic stuff}

\begin{code}
-- | Sort semantics first according to its predicate, and then to its handles.
sortSem :: Sem -> Sem
sortSem = sortBy (\(h1,p1,a1) (h2,p2,a2) -> compare (p1, h1:a1) (p2, h2:a2))
\end{code}

% --------------------------------------------------------------------
\subsection{Feature structure unification}
\label{sec:fs_unification}
% --------------------------------------------------------------------

Feature structure unification takes two feature lists as input.  If it
fails, it returns Nothing.  Otherwise, it returns a tuple with:

\begin{enumerate}
\item a unified feature structure list
\item a list of variable replacements that will need to be propagated
      across other feature structures with the same variables
\end{enumerate}

Unification fails if, at any point during the unification process, the
two lists have different constant values for the same attribute.
For example, unification fails on the following inputs because they have
different values for the \textit{number} attribute:

\begin{quotation}
\fs{\it cat:np\\ \it number:3\\}
\fs{\it cat:np\\ \it number:2\\}
\end{quotation}

Note that the following input should also fail as a result on the
coreference on \textit{?X}.

\begin{quotation}
\fs{\it cat:np\\ \it one: 1\\  \it two:2\\}
\fs{\it cat:np\\ \it one: ?X\\ \it two:?X\\}
\end{quotation}

On the other hand, any other pair of feature lists should unify
succesfully, even those that do not share the same attributes.
Below are some examples of successful unifications:

\begin{quotation}
\fs{\it cat:np\\ \it one: 1\\  \it two:2\\}
\fs{\it cat:np\\ \it one: ?X\\ \it two:?Y\\}
$\rightarrow$
\fs{\it cat:np\\ \it one: 1\\ \it two:2\\},
\end{quotation}

\begin{quotation}
\fs{\it cat:np\\ \it number:3\\}
\fs{\it cat:np\\ \it case:nom\\}
$\rightarrow$
\fs{\it cat:np\\ \it case:nom\\ \it number:3\\},
\end{quotation}

\begin{code}
-- | 'unifyFeat' performs feature structure unification, under the
--   these assumptions about the input:
--
--    * Features are ordered
--
--    * The Flists do not share variables (renaming has already
--      been done.
--
--   The features are allowed to have different sets of attributes,
--   beacuse we use 'alignFeat' to realign them.
unifyFeat :: Monad m => Flist -> Flist -> m (Flist, Subst)
unifyFeat f1 f2 =
  {-# SCC "unification" #-}
  let (att, val1, val2) = unzip3 $ alignFeat f1 f2
  in att `seq`
     do (res, subst) <- unify val1 val2
        return (zipWith AvPair att res, subst)

-- | 'alignFeat' is a pre-procesing step used to ensure that feature structures
--   have the same set of keys.  If a key is missing in one, we copy it to the
--   other with an anonymous value.
--
--   The two feature structures must be sorted for this to work
alignFeat :: Flist -> Flist -> [(String,GeniVal,GeniVal)]
alignFeat f1 f2 = alignFeatH f1 f2 []

alignFeatH :: Flist -> Flist -> [(String,GeniVal,GeniVal)] -> [(String,GeniVal,GeniVal)]
alignFeatH [] [] acc = reverse acc
alignFeatH [] (AvPair f v :x) acc = alignFeatH [] x ((f,GAnon,v) : acc)
alignFeatH x [] acc = alignFeatH [] x acc
alignFeatH fs1@(AvPair f1 v1:l1) fs2@(AvPair f2 v2:l2) acc =
   case compare f1 f2 of
     EQ -> alignFeatH l1 l2  ((f1, v1, v2) : acc)
     LT -> alignFeatH l1 fs2 ((f1, v1, GAnon) : acc)
     GT -> alignFeatH fs1 l2 ((f2, GAnon, v2) : acc)
\end{code}


