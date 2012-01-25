-- ----------------------------------------------------------------------
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
-- ----------------------------------------------------------------------

module NLP.GenI.Test where

import Data.List ( isPrefixOf )
import System.Environment ( getArgs )
import Test.Framework
import NLP.GenI.Test.FeatureStructures ( suite )
import NLP.GenI.Test.GeniParsers ( suite )
import NLP.GenI.Test.GeniVal ( suite )
import NLP.GenI.Test.Lexicon ( suite )
import NLP.GenI.Test.Morphology ( suite )
import NLP.GenI.Test.Polarity ( suite )
import NLP.GenI.Test.Semantics ( suite )
import NLP.GenI.Test.Simple.SimpleBuilder ( suite )
import NLP.GenI.Regression

runTests :: IO ()
runTests =
 do args <- filter (not . (`isPrefixOf` "--unit-tests")) `fmap` getArgs
    funcSuite <- NLP.GenI.Regression.mkSuite
    flip defaultMainWithArgs args
     [ NLP.GenI.Test.GeniVal.suite
     , NLP.GenI.Test.GeniParsers.suite
     , NLP.GenI.Test.FeatureStructures.suite
     , NLP.GenI.Test.Lexicon.suite
     , NLP.GenI.Test.Morphology.suite
     , NLP.GenI.Test.Polarity.suite
     , NLP.GenI.Test.Semantics.suite
     , NLP.GenI.Test.Simple.SimpleBuilder.suite
     , funcSuite
     ]
