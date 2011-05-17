-- GenI surface realiser
-- Copyright (C) 2009 Eric Kow
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

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module NLP.GenI.PolarityTypes where

import qualified Data.Set as Set
import Data.Generics ( Data )
import Data.Typeable ( Typeable )

import Control.DeepSeq

newtype PolarityKey = PolarityKey { fromPolarityKey :: String } deriving (Show, Eq, Ord, Data, Typeable)
type SemPols  = [Int]

-- | 'PolarityAttr' is something you want to perform detect polarities on.
data PolarityAttr = SimplePolarityAttr { spkAtt :: String }
 -- | 'RestrictedPolarityKey' @c att@ is a polarity key in which we only pay
 --   attention to nodes that have the category @c@.  This makes it possible
 --   to have polarities for a just a small subset of nodes
 | RestrictedPolarityAttr { _rpkCat :: String, rpkAtt :: String }
 deriving (Eq, Ord, Typeable)

readPolarityAttrs :: String -> Set.Set PolarityAttr
readPolarityAttrs = Set.fromList . map helper . words
 where
  helper s = case break (== '.') s of
             (a,"") -> SimplePolarityAttr a
             (c,a)  -> RestrictedPolarityAttr c (drop 1 a)

showPolarityAttrs :: Set.Set PolarityAttr -> String
showPolarityAttrs = unwords . map show . Set.toList

instance Show PolarityAttr where
 show (SimplePolarityAttr a) = a
 show (RestrictedPolarityAttr c a) = c ++ "." ++ a

{-!
deriving instance NFData PolarityKey
deriving instance NFData PolarityAttr
!-}
-- GENERATED START

 
instance NFData PolarityKey where
        rnf (PolarityKey x1) = rnf x1 `seq` ()

 
instance NFData PolarityAttr where
        rnf (SimplePolarityAttr x1) = rnf x1 `seq` ()
        rnf (RestrictedPolarityAttr x1 x2) = rnf x1 `seq` rnf x2 `seq` ()
-- GENERATED STOP
