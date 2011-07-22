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

module NLP.GenI.Polarity.Internal where

import NLP.GenI.FeatureStructures
import NLP.GenI.General
import NLP.GenI.GeniVal
import NLP.GenI.PolarityTypes

data PolarityDetectionResult = PD_UserError String
                             | PD_Nothing
                             | PD_Just [ (PolarityKey, Interval) ]
                             | PD_Unconstrained (String, Interval)

-- ----------------------------------------------------------------------
-- polarity detection
-- ----------------------------------------------------------------------

detectPolarityForAttr :: Int -- ^ polarity to assign
                      -> String
                      -> Flist GeniVal
                      -> PolarityDetectionResult
detectPolarityForAttr i att fl =
  case [ v | AvPair a v <- fl, a == att ] of
    []  -> PD_Unconstrained (withZero att)
    [v] -> case gConstraints v of
             Just [x] -> PD_Just [ (PolarityKeyAv att x, ival i) ]       -- singleton
             Just xs  -> PD_Just $ map (withZero . PolarityKeyAv att) xs   -- interval if ambiguous
             Nothing  -> PD_Unconstrained (withZero att)
    _   -> PD_UserError $ "[polarities] More than one value for attribute: " ++ att ++ " in:" ++ showFlist fl
 where
   withZero x = (x, toZero i)

toZero :: Int -> Interval
toZero x | x < 0     = (x, 0)
         | otherwise = (0, x)
