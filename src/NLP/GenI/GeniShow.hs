--  GenI surface realiser
--  Copyright (C) 2005 Carlos Areces and Eric Kow
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 2
--  of the License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

-- This module provides specialised functions for visualising tree data.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.GenI.GeniShow
where

import Data.Text ( Text )
import Data.Tree
import qualified Data.Text as T
import NLP.GenI.Pretty

-- | GenI format; should round-trip with 'NLP.GenI.Parser' by rights
--
--   Minimal definition, either one of 'geniShow' or 'geniShowText'
class GeniShow a where
    geniShow :: a -> String
    geniShow = T.unpack . geniShowText

    geniShowText :: a -> Text
    geniShowText = T.pack . geniShow

instance (GeniShow a) => GeniShow (Tree a) where
    geniShowText = geniShowTree 0

geniShowTree :: GeniShow a => Int -> Tree a -> Text
geniShowTree i (Node a l) =
    spaces <> geniShowText a <> rest
  where
    rest = case (l,i) of
               ([], 0) -> "{}"
               ([], _) -> ""
               (_,  _) -> "{\n" <> T.unlines (map next l)
                                <> spaces
                                <> "}"
    next   = geniShowTree (i+1)
    spaces = T.pack (replicate i ' ')

{-
geniShowSmallList :: GeniShow a => [a] -> String
geniShowSmallList = squares . unwords . (map geniShow)

instance GeniShow [Literal] where
 geniShow = geniShowSmallList

instance GeniShow (AvPair v) => GeniShow [AvPair v] where
 geniShow = geniShowSmallList

-}

geniKeyword :: Text -> Text  -> Text
geniKeyword k t = k `T.append` ":" `T.append` t
