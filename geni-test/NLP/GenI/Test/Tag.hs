-- GenI surface realiser
-- Copyright (C) 2011 SRI
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module NLP.GenI.Test.Tag where

import qualified Data.Map as Map

import Test.HUnit
import Test.QuickCheck hiding (collect, Failure)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import NLP.GenI.FeatureStructure
import NLP.GenI.GeniVal
import NLP.GenI.Semantics
import NLP.GenI.TreeSchema
import NLP.GenI.Tag

import NLP.GenI.Test.TreeSchema hiding ( suite )

suite :: Test.Framework.Test
suite =
 testGroup "NLP.GenI.Tag"
  []

sillyTagElem :: TagElem
sillyTagElem = TE
  { idname     = "silly"
  , ttreename  = "silly"
  , tidnum     = 0
  , ttree      = sillyTree
  , ttype      = Initial
  , tsemantics = [ sillyLiteral ]
  , tpolarities = Map.empty
  , tinterface  = []
  , ttrace      = []
  , tsempols    = []
  }

sillyTagElemAux :: TagElem
sillyTagElemAux = sillyTagElem
  { idname     = "sillyaux"
  , ttreename  = "sillyaux"
  , ttree      = sillyTreeAux
  , ttype      = Auxiliar
  }

sillyLiteral :: Literal
sillyLiteral = Literal (mkGConstNone "l") (mkGConstNone "silly") []
