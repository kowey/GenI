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

-- ----------------------------------------------------------------------
-- polarity detection
-- ----------------------------------------------------------------------

detectPolarityForAttr :: Int -- ^ polarity to assign
                      -> String
                      -> Flist GeniVal
                      -> PolarityDetectionResult
detectPolarityForAttr i att fl =
  case [ v | AvPair a v <- fl, a == att ] of
    []  -> PD_UserError $ "[polarities] No value for attribute: " ++ att ++ " in:" ++ showFlist fl
    [v] -> case gConstraints v of
             Just _  -> PD_Just $ case prefixWith att (values v) of
                                    [x] -> [ (PolarityKey x, ival i) ]                -- singleton
                                    xs  -> map (\x -> (PolarityKey x, toZero i)) xs   -- interval if ambiguous
             -- EYK goal is to add unconstrained values here
             -- how do we ensure that we only receive constants here?
             Nothing -> PD_UserError $ "[polarities] Non-constrained value for attribute: " ++ att ++ " in:" ++ showFlist fl
    _   -> PD_UserError $ "[polarities] More than one value for attribute: " ++ att ++ " in:" ++ showFlist fl
 where
  values v = case gConstraints v of
               Nothing -> geniBug $ "detectPolarityForAttr: no constraints for constant?! " ++ show v
               Just x  -> x

toZero :: Int -> Interval
toZero x | x < 0     = (x, 0)
         | otherwise = (0, x)

prefixWith :: String -> [String] -> [String]
prefixWith att = map (\x -> att ++ ('_' : x))
