-- GenI surface realiser
-- Copyright (C) 2012 Eric Kow
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

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This is not a proper pretty printer. I aim is to replace this with a
--   (de-facto) standard library if one should appear
module NLP.GenI.Pretty where

import Data.Text ( Text )
import qualified Data.Text as T

-- | An alternative 'Show' instance (the idea being that we
--   should reserve 'Show' for outputting actual Haskell)
--
--   Minimal implementation is 'pretty' or 'prettyStr'
class Pretty a where
   pretty :: a -> Text
   pretty = T.pack . prettyStr

   prettyStr :: a -> String
   prettyStr = T.unpack . pretty

instance Pretty String where
   prettyStr a = a

between :: Text -> Text -> Text -> Text
between l r x = l `T.append` x `T.append` r

parens :: Text -> Text
parens = between "(" ")"

squares :: Text -> Text
squares = between "[" "]"
