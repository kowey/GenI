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

module NLP.GenI.LexicalSelection.Types where

import Control.Monad.Writer
import Data.List
import Data.List.Split ( wordsBy )
import Data.Poset
import Data.Text ( Text )
import qualified Data.Map as Map
import qualified Data.Text as T

import NLP.GenI.GeniVal
import NLP.GenI.General ( showWithCount )
import NLP.GenI.TreeSchemata

-- | Left hand side of a path equation
data PathEqLhs = PeqInterface   Text
               | PeqJust        NodePathEqLhs
               | PeqUnknown     Text
  deriving (Eq, Ord)

-- ----------------------------------------------------------------------
-- * Path equations
-- ----------------------------------------------------------------------

-- | Path equations can either hit a feature or a node's lexeme attribute
data NodePathEqLhs = PeqFeat String TopBottom Text
                   | PeqLex  String
  deriving (Eq, Ord)

data TopBottom = Top | Bottom
  deriving (Eq, Ord)

type PathEqPair = (NodePathEqLhs, GeniVal)

-- | Parse a path equation using the GenI conventions
--   This always succeeds, but can return @Just warning@
--   if anything anomalous comes up
--   FIXME : make more efficient
parsePathEq :: Text -> Writer [LexCombineError] PathEqLhs
parsePathEq e =
  case wordsBy (== '.') (T.unpack e) of
  (n:"top":r)     -> return (node n Top    r)
  (n:"bot":r)     -> return (node n Bottom r)
  [n,"lex"]       -> return (PeqJust (PeqLex n))
  ("top":r)       -> return (node "anchor" Top r)
  ("bot":r)       -> return (node "anchor" Bottom r)
  ("anchor":r)    -> return (node "anchor" Bottom r)
  ("interface":r) -> return (PeqInterface     (rejoin r))
  ("anc":r)       -> parsePathEq $ rejoin ("anchor":r)
  (n:r@(_:_))     -> tell [ BoringError (tMsg n) ] >> return (node n Top r)
  _               -> tell [ BoringError iMsg     ] >> return (PeqUnknown e)
 where
  node n tb r = PeqJust $ PeqFeat n tb (rejoin r)
  rejoin = T.pack . concat . intersperse "."
  tMsg n = "Interpreting path equation " ++ T.unpack e ++ " as applying to top of " ++ n ++ "."
  iMsg   = "Could not interpret path equation " ++ T.unpack e

showPathEqLhs :: PathEqLhs -> String
showPathEqLhs p =
  case p of
   PeqJust (PeqFeat n tb att) -> squish [ n, fromTb tb, T.unpack att ]
   PeqJust (PeqLex  n)        -> squish [ n, "lex" ]
   PeqInterface att -> squish [ "interface", T.unpack att ]
   PeqUnknown e     -> T.unpack e
 where
  fromTb Top    = "top"
  fromTb Bottom = "bot"
  squish = intercalate "."

-- ----------------------------------------------------------------------
-- * Warnings
-- ----------------------------------------------------------------------

data LexCombineError =
       BoringError String
     | FamilyNotFoundError String
     | SchemaError [SchemaTree] LexCombineError2
 deriving Eq

data LexCombineError2 = EnrichError PathEqLhs
                      | StringError String
 deriving (Eq, Ord)

instance Poset LexCombineError where
 leq (BoringError _) _                                 = True
 leq (SchemaError _ e1) (SchemaError _ e2)             = leq e1 e2
 leq (FamilyNotFoundError x1) (FamilyNotFoundError x2) = leq x1 x2
 leq (FamilyNotFoundError _)  (SchemaError _ _)        = True
 leq _ _ = False

instance Poset LexCombineError2 where
 leq (EnrichError _)  (EnrichError _ ) = False
 leq (EnrichError _ ) (StringError _ ) = True
 leq (StringError s1) (StringError s2) = leq s1 s2
 leq _ _ = False


instance Show LexCombineError where
 show e = body ++ suffix
  where
   (body, suffix) = showLexCombineError e

showLexCombineError :: LexCombineError -> (String, String)
showLexCombineError (SchemaError xs x) = (show x, showWithCount (const "") "trees" ((),length xs))
showLexCombineError (BoringError s)    = (s, "")
showLexCombineError (FamilyNotFoundError f) = ("Family " ++ f ++ " not found in tree schema file", "")

instance Show LexCombineError2 where
 show (EnrichError p) = "Some trees discarded due enrichment error on " ++ showPathEqLhs p
 show (StringError s) = s

compressLexCombineErrors :: [LexCombineError] -> [LexCombineError]
compressLexCombineErrors errs = schema2 ++ normal
 where
  isSchema (SchemaError _ _) = True
  isSchema _ = False
  (schema, normal) = partition isSchema errs
  schema2 = map (uncurry (flip SchemaError))
          . Map.toList
          $ Map.fromListWith (++) [ (l,ts) | SchemaError ts l <- schema ]
