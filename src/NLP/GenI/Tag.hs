-- GenI surface realiser
-- Copyright (C) 2005 Carlos Areces and Eric Kow
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

-- | This module provides basic datatypes specific to Tree Adjoining Grammar
-- (TAG) elementary trees and some low-level operations.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module NLP.GenI.Tag (
   -- Main Datatypes
   Tags, TagElem(..), TagItem(..), TagSite(..),
   TagDerivation, DerivationStep(..), dsChild, dsParent, dsParentSite,
   ts_synIncomplete, ts_semIncomplete, ts_tbUnificationFailure,
   ts_rootFeatureMismatch,

   -- Functions from Tags
   addToTags, tagLeaves, getLexeme, toTagSite,

   -- Functions from TagElem
   setTidnums, plugTree, spliceTree,

   -- General functions
   mapBySem,
   collect, detectSites,
) where

import Control.Applicative ( (<$>), (<*>) )
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, catMaybes)
import Data.Tree
import Data.Text ( Text )
import qualified Data.Text as T

import Control.DeepSeq
import Data.Generics (Data)
import Data.Typeable (Typeable)
import Data.FullList hiding ( (++) )
import Text.JSON

import NLP.GenI.FeatureStructure ( AvPair(..), Flist )
import NLP.GenI.General (listRepNode, groupByFM, preTerminals, geniBug)
import NLP.GenI.GeniShow
import NLP.GenI.GeniVal ( GeniVal(..), DescendGeniVal(..), Collectable(..), Idable(..), )
import NLP.GenI.Polarity.Types ( PolarityKey(..), SemPols )
import NLP.GenI.Pretty
import NLP.GenI.Semantics ( Sem, Literal(..), emptyLiteral )
import NLP.GenI.TreeSchema
    ( Ptype(..), GNode(..), GType(..), NodeName, lexemeAttributes )

-- ----------------------------------------------------------------------
-- Tags
-- ----------------------------------------------------------------------

-- | An anchored grammar.
--   The grammar associates a set of semantic predicates to a list of trees each.
type Tags = Map.Map String [TagElem]                            

-- | 'addTags' @tags key elem@ adds @elem@ to the the list of elements associated
--   to the key
addToTags :: Tags -> String -> TagElem -> Tags
addToTags t k e = Map.insertWith (++) k [e] t

-- ----------------------------------------------------------------------
-- TagElem
-- ----------------------------------------------------------------------

data TagSite = TagSite
    { tsName :: Text
    , tsUp   :: Flist GeniVal
    , tsDown :: Flist GeniVal
    , tsOrigin :: Text
    }
  deriving (Eq, Ord, Data, Typeable)

data TagElem = TE
    { idname       :: Text
    , ttreename    :: Text
    , tidnum       :: Integer
    , ttype        :: Ptype
    , ttree        :: Tree (GNode GeniVal)
    , tsemantics   :: Sem
     -- optimisation stuff
     -- (polarity key to charge interval)
    , tpolarities  :: Map.Map PolarityKey (Int,Int)
    , tinterface   :: Flist GeniVal  -- for idxconstraints (pol)
    , ttrace       :: [Text]
    , tsempols     :: [SemPols] -- ^ can be empty
    }
  deriving (Eq, Data, Typeable)

-- | Given a tree(GNode) returns a list of substitution or adjunction
--   nodes, as well as remaining nodes with a null adjunction constraint.
detectSites :: Tree (GNode GeniVal) -> ([NodeName], [NodeName], [NodeName])
detectSites t =
  ( sites isSub           -- for substitution
  , sites (not.gaconstr)  -- for adjunction
  , sites constrButNotSub -- for neither
  )
 where
 ns = flatten t
 sites match = map gnname . filter match $ ns
 isSub n = gtype n == Subs
 constrButNotSub n = gaconstr n && (not $ isSub n)

toTagSite :: GNode GeniVal -> TagSite
toTagSite n = TagSite (gnname n) (gup n) (gdown n) (gorigin n)

type TagDerivation = [ DerivationStep ]

data DerivationStep = SubstitutionStep Text Text Text
                    | AdjunctionStep   Text Text Text
                    | InitStep         Text
 deriving (Show, Ord, Eq)

dsOp :: DerivationStep -> Char
dsOp (SubstitutionStep {}) = 's'
dsOp (AdjunctionStep {})   = 'a'
dsOp (InitStep {})         = 'i'

dsChild :: DerivationStep -> Text
dsChild (SubstitutionStep c _ _) = c
dsChild (AdjunctionStep c _ _ )  = c
dsChild (InitStep c)             = c

dsParent :: DerivationStep -> Maybe Text
dsParent (SubstitutionStep _ p _) = Just p
dsParent (AdjunctionStep _ p _)   = Just p
dsParent (InitStep _)             = Nothing

dsParentSite :: DerivationStep -> Maybe Text
dsParentSite (SubstitutionStep _ _ s) = Just s
dsParentSite (AdjunctionStep _ _ s)   = Just s
dsParentSite (InitStep _)             = Nothing

instance JSON DerivationStep where
 readJSON j = do
   jo <- fromJSObject `fmap` readJSON j
   let field x = maybe (fail $ "Could not find: " ++ x) readJSON
               $ lookup x jo
   op    <- field "op"
   child <- field "child"
   case op of
    "s" -> AdjunctionStep   child <$> field "parent" <*> field "parent-node"
    "a" -> SubstitutionStep child <$> field "parent" <*> field "parent-node"
    "i" -> return (InitStep child)
    x   -> fail $ "Don't know about derivation operation '" ++ x ++ "'"
 showJSON x =
     JSObject . toJSObject $ [ ("op",     showJSON $ dsOp x)
                             , ("child",  showJSON $ dsChild x)
                             ] ++ catMaybes
                             [ (\v -> ("parent", showJSON v))      <$> dsParent x
                             , (\v -> ("parent-node", showJSON v)) <$> dsParentSite x
                             ]

instance Ord TagElem where
  compare t1 t2 = 
    case (ttype t1, ttype t2) of
         (Initial, Initial)   -> compareId 
         (Initial, Auxiliar)  -> LT
         (Auxiliar, Initial)  -> GT
         (Auxiliar, Auxiliar) -> compareId 
    where compareId  = compare (tidnum t1) (tidnum t2)

instance DescendGeniVal TagElem where
  descendGeniVal s te =
    te { tinterface = descendGeniVal s (tinterface te)
       , ttree      = descendGeniVal s (ttree te)
       , tsemantics = descendGeniVal s (tsemantics te) }

instance DescendGeniVal TagSite where
  descendGeniVal s (TagSite n fu fd o) = TagSite n (descendGeniVal s fu) (descendGeniVal s fd) o

instance Collectable TagElem where
  collect t = (collect $ tinterface t) . (collect $ ttree t) 
            . (collect $ tsemantics t)

instance Idable TagElem where
  idOf = tidnum

-- Substitution and Adjunction

-- | Plug the first tree into the second tree at the specified node.
--   Anything below the second node is silently discarded.
--   We assume the trees are pluggable; it is treated as a bug if
--   they are not!
plugTree :: Tree NodeName
         -> NodeName
         -> Tree NodeName
         -> Tree NodeName
plugTree male n female =
    case listRepNode (const male) (nmatch n) [female] of
         ([r], True) -> r
         _           -> geniBug oops
  where
    oops = "plugTree: unexpected plug failure at node " ++ T.unpack n

-- | Given two trees 'auxt' and 't', splice the tree 'auxt' into
--   't' via the TAG adjunction rule.
spliceTree :: NodeName      -- ^ foot node of the aux tree
           -> Tree NodeName -- ^ aux tree
           -> NodeName      -- ^ place to adjoin in target tree
           -> Tree NodeName -- ^ target tree
           -> Tree NodeName
spliceTree f auxT n top =
    plugTree middle n top 
  where
    bottom = fromMaybe (geniBug oops) (findSubTree n top)
    middle = plugTree bottom f auxT
    oops = unwords
        [ "NLP.GenI.Tag.spliceTree:"
        , "Unexpected adjunction failure."
        , "Could not find node " ++ T.unpack n ++ " of target tree."
        ]

nmatch :: NodeName -> Tree NodeName -> Bool
nmatch n (Node a _) = a == n

findSubTree :: NodeName -> Tree NodeName -> Maybe (Tree NodeName)
findSubTree n n2@(Node x ks)
  | x == n    = Just n2
  | otherwise = case mapMaybe (findSubTree n) ks of
                []    -> Nothing
                (h:_) -> Just h

-- Unique ID

-- | Assigns a unique id to each element of this list, that is, an integer
--   between 1 and the size of the list.
setTidnums :: [TagElem] -> [TagElem]
setTidnums xs = zipWith (\c i -> setOrigin $ c {tidnum = i}) xs [1..]

setOrigin :: TagElem -> TagElem
setOrigin te =
    te { ttree = fmap setLabel . ttree $ te }
  where
    setLabel g = g { gorigin = nameNumber }
    nameNumber = idname te
        `T.append` ":"
        `T.append` (T.pack . show . tidnum) te

-- ----------------------------------------------------------------------
-- TAG Item
-- ----------------------------------------------------------------------

-- | 'TagItem' is a generalisation of 'TagElem'.
class TagItem t where 
    tgIdName    :: t -> Text
    tgIdNum     :: t -> Integer
    tgSemantics :: t -> Sem
    tgTree      :: t -> Tree (GNode GeniVal)

instance TagItem TagElem where
    tgIdName = idname
    tgIdNum  = tidnum
    tgSemantics = tsemantics
    tgTree = ttree

-- | Sorts trees into a Map.Map organised by the first literal of their
--   semantics.  This is useful in at least three places: the polarity
--   optimisation, the gui display code, and code for measuring the efficiency
--   of GenI.  Note: trees with a null semantics are filed under an empty
--   predicate, if any.
mapBySem :: (TagItem t) => [t] -> Map.Map (Literal GeniVal) [t]
mapBySem ts = 
  let gfn t = case tgSemantics t of
              []    -> emptyLiteral
              (x:_) -> x
  in groupByFM gfn ts

-- ----------------------------------------------------------------------
-- Extracting sentences
-- ----------------------------------------------------------------------

type UninflectedDisjunction = ([Text], Flist GeniVal)

-- | Normally, extracting the sentences from a TAG tree would just
--   consist of reading its leaves.  But if you want the generator to
--   return inflected forms instead of just lemmas, you also need to
--   return the relevant features for each leaf.  In TAG, or at least our
--   use of it, the features come from the *pre-terminal* nodes, that is,
--   not the leaves themselves but their parents.  Another bit of
--   trickiness: because of atomic disjunction, leaves might have more
--   than one value, so we can't just return a String lemma but a list of
--   String, one for each possibility.
tagLeaves :: TagElem -> [ (NodeName, UninflectedDisjunction) ]
tagLeaves te = [ (gnname pt, (getLexeme t, gup pt)) | (pt,t) <- preTerminals . ttree $ te ]

-- | Try in order: lexeme, lexeme attributes, node name
getLexeme :: GNode GeniVal -> [Text]
getLexeme node =
    case glexeme node of
        []   -> fromMaybe [gnname node] $ firstMaybe grab lexemeAttributes
        lexs -> lexs
  where
    grab la = firstMaybe match guppy
      where
        match (AvPair a v) | a == la = fromFL <$> gConstraints v
        match _ = Nothing
    guppy = gup node

firstMaybe :: (a -> Maybe b) -> [a] -> Maybe b
firstMaybe fn = listToMaybe . mapMaybe fn

-- ----------------------------------------------------------------------
-- Conversion to text
-- ----------------------------------------------------------------------

instance GeniShow TagElem where
    geniShowText te = T.concat
        [ "% ------------------------- ", idname te
        , "\n", ttreename te, ":" , idname te
        , " " , (geniShowText $ tinterface te)
        , " " , (geniShowText $ ttype te)
        , "\n", (geniShowText $ ttree te)
        , "\n", geniKeyword "semantics" (geniShowText $ tsemantics te)
        ]

instance GeniShow [TagElem] where
    geniShowText = T.intercalate "\n\n" . map geniShowText

-- Useful for debugging adjunction and substitution nodes
instance Pretty [TagSite] where
    pretty =
        T.intercalate "\n  " . map fn
      where
        fn (TagSite n t b o) = T.intercalate "/"
            [ n, pretty t, pretty b, o ]

-- ----------------------------------------------------------------------
-- Diagnostic messages
--
-- Diagnostic messages let us know why a TAG tree is not returned as a result.
-- Whenever GenI decides to discard a tree, it sets the tdiagnostic field of 
-- the TagElem so that the person using a debugger can find out what went wrong.
-- ----------------------------------------------------------------------

ts_synIncomplete, ts_tbUnificationFailure :: String
ts_synIncomplete = "syntactically incomplete"
ts_tbUnificationFailure = "top/bot unification failure"

ts_rootFeatureMismatch :: Flist GeniVal -> String
ts_rootFeatureMismatch good = "root feature does not unify with " ++ prettyStr good

ts_semIncomplete :: [Literal GeniVal] -> String
ts_semIncomplete sem = "semantically incomplete - missing:  " ++ prettyStr sem

-- ----------------------------------------------------------------------
-- Performance
-- ----------------------------------------------------------------------

{-!
deriving instance NFData TagElem
deriving instance NFData DerivationStep
!-}

-- GENERATED START

 
instance NFData TagElem where
        rnf (TE x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
          = rnf x1 `seq`
              rnf x2 `seq`
                rnf x3 `seq`
                  rnf x4 `seq`
                    rnf x5 `seq`
                      rnf x6 `seq`
                        rnf x7 `seq` rnf x8 `seq` rnf x9 `seq` rnf x10 `seq` ()

 
instance NFData DerivationStep where
        rnf (SubstitutionStep x1 x2 x3)
          = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` ()
        rnf (AdjunctionStep x1 x2 x3)
          = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` ()
        rnf (InitStep x1) = rnf x1 `seq` ()
-- GENERATED STOP
