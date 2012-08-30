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
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Feature structures in GenI can be seen as a simple mapping from
--   attributes to values (no fancy recursion).
--
--   From an implementation standpoint, we do truck around lists of
--   'AvPair' quite a bit which unfortunately means we don't
--   guarantee things like uniqueness of attributes.  We may phase
--   this out over time in favour of 'FeatStruct'
module NLP.GenI.FeatureStructure where

import Data.Binary
import Data.Function (on)
import Data.Generics (Data)
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Typeable (Typeable)
import Data.Text ( Text )
import qualified Data.Text as T

import NLP.GenI.GeniShow
import NLP.GenI.GeniVal
import NLP.GenI.General ( geniBug )
import NLP.GenI.Pretty

import Control.DeepSeq

-- ----------------------------------------------------------------------
-- Core types
-- ----------------------------------------------------------------------

-- | A list of attribute-value pairs. It's not a great idea to represent
--   feature structures with this because it allows for duplicates in the
--   attributes. But maybe sometimes you really do mean a list.
type Flist a  = [AvPair a]

-- | An attribute-value pair, the typical use being
--   @AvPair GeniVal@ or if you have something even simpler
--   @AvPair Text@
data AvPair a = AvPair { avAtt :: Text
                       , avVal :: a }
  deriving (Ord, Eq, Data, Typeable)

-- | Experimental, alternative representation of Flist
--   which guarantees uniqueness of keys
type FeatStruct a = Map.Map Text a

-- | A feature structure with no pairs
emptyFeatStruct :: FeatStruct a
emptyFeatStruct = Map.empty

-- | Convert an 'Flist' to a proper 'FeatStruct'
--   Unsafely assumes the keys are unique
mkFeatStruct :: Flist GeniVal -> FeatStruct GeniVal 
mkFeatStruct fs = Map.fromListWith oops . map fromPair $ fs
  where
   fromPair (AvPair a v) = (a,v)
   oops _ _ = geniBug $
     "I've allowed a feature structure with multiple versions of a key"
     ++ " to sneak through: " ++ prettyStr fs

-- | Convert an 'FeatStruct' to a simpler to process 'Flist'
fromFeatStruct :: FeatStruct a -> Flist a
fromFeatStruct = sortFlist . map (uncurry AvPair) . Map.toList

-- if we decide to move over to this representation of feature structures
-- we can get rid of showFlist, etc and probably just use toAscList
instance Pretty (FeatStruct GeniVal) where
    pretty = pretty . fromFeatStruct

instance GeniShow (FeatStruct GeniVal) where
    geniShowText = geniShowText . fromFeatStruct

-- ----------------------------------------------------------------------
-- Basic functions
-- ----------------------------------------------------------------------

-- | Sort an Flist according with its attributes
sortFlist :: Flist a -> Flist a
sortFlist = sortBy (compare `on` avAtt)

-- Traversal

instance DescendGeniVal v => DescendGeniVal (AvPair v) where
  descendGeniVal s (AvPair a v) = {-# SCC "descendGeniVal" #-} AvPair a (descendGeniVal s v)

instance DescendGeniVal a => DescendGeniVal (String, a) where
  descendGeniVal s (n,v) = {-# SCC "descendGeniVal" #-} (n,descendGeniVal s v)

instance DescendGeniVal v => DescendGeniVal ([String], Flist v) where
  descendGeniVal s (a,v) = {-# SCC "descendGeniVal" #-} (a, descendGeniVal s v)

instance Collectable a => Collectable (AvPair a) where
  collect (AvPair _ b) = collect b

-- Pretty printing and output format

instance Pretty (Flist GeniVal) where
    pretty = geniShowText

instance Pretty (AvPair GeniVal) where
    pretty = geniShowText

instance GeniShow (Flist GeniVal) where
    geniShowText = squares . T.unwords . map geniShowText

instance GeniShow (AvPair GeniVal) where
    geniShowText (AvPair a v) = a `T.append` ":" `T.append` geniShowText v

{-
instance Show (AvPair GeniVal) where
  show = showAv
-}

-- --------------------------------------------------------------------
-- Feature structure unification
-- --------------------------------------------------------------------

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
unifyFeat :: Monad m => Flist GeniVal -> Flist GeniVal -> m (Flist GeniVal, Subst)
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
alignFeat :: Flist GeniVal -> Flist GeniVal -> [(Text,GeniVal,GeniVal)]
alignFeat f1 f2 = alignFeatH f1 f2 []

-- | Helper for 'alignFeat'; ignore
alignFeatH :: Flist GeniVal -> Flist GeniVal -> [(Text,GeniVal,GeniVal)] -> [(Text,GeniVal,GeniVal)]
alignFeatH [] [] acc = reverse acc
alignFeatH [] (AvPair f v :x) acc = alignFeatH [] x ((f,mkGAnon,v) : acc)
alignFeatH x [] acc = alignFeatH [] x acc
alignFeatH fs1@(AvPair f1 v1:l1) fs2@(AvPair f2 v2:l2) acc =
   case compare f1 f2 of
     EQ -> alignFeatH l1 l2  ((f1, v1, v2) : acc)
     LT -> alignFeatH l1 fs2 ((f1, v1, mkGAnon) : acc)
     GT -> alignFeatH fs1 l2 ((f2, mkGAnon, v2) : acc)

-- --------------------------------------------------------------------
-- Fancy disjunction
-- --------------------------------------------------------------------

-- | Flatten a fancy disjunction attribute-value pair
--
--   See 'crushOne' for details
crushAvPair :: AvPair SchemaVal -> Maybe (AvPair GeniVal)
crushAvPair (AvPair a v) = AvPair a `fmap` crushOne v

-- | Flatten a fancy-disjunction feature structure
--
--   See 'crushOne' for details
crushFlist :: Flist SchemaVal -> Maybe (Flist GeniVal)
crushFlist = mapM crushAvPair

{-!
deriving instance Binary AvPair
deriving instance NFData AvPair
!-}

-- GENERATED START


instance (Binary a) => Binary (AvPair a) where
        put (AvPair x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (AvPair x1 x2)

 
instance (NFData a) => NFData (AvPair a) where
        rnf (AvPair x1 x2) = rnf x1 `seq` rnf x2 `seq` ()
-- GENERATED STOP
