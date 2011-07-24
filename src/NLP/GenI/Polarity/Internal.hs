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

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Maybe (isJust)
import Data.Tree (flatten)

import NLP.GenI.Automaton
import NLP.GenI.Btypes(Pred,
              Ptype(Initial),
              GNode, root, gup, gdown, gtype, GType(Subs),
              )
import NLP.GenI.Tags(TagElem(..), TagItem(..))

data PolarityDetectionResult = PD_UserError String
                             | PD_Nothing
                             | PD_Just [ (PolarityKey, Interval) ]
                             | PD_Unconstrained (String, Interval)

-- ----------------------------------------------------------------------
-- polarity detection
-- ----------------------------------------------------------------------

detectPolsH :: Set.Set PolarityAttr -> TagElem -> TagElem
detectPolsH polarityAttrs te =
  let detectOrBust x1 x2 x3 x4 =
        case detectPolarity x1 x2 x3 x4 of
        PD_UserError e -> error $ e ++ " in " ++ tgIdName te -- ideally we'd propagate this
        PD_Nothing     -> []
        PD_Just p      -> p
        PD_Unconstrained (att, _) -> error $ "[polarities] Non-constrained value for attribute: " ++ att
      --
      rup   = mkFeatStruct . gup . root .ttree $ te
      rdown = mkFeatStruct . gdown . root . ttree $ te
      --
      catAttr = SimplePolarityAttr "cat"
      rstuffLite  = concatMap (\v -> detectOrBust 1 v rup rdown)
                  $ Set.toList $ Set.delete catAttr polarityAttrs
      rstuff :: [(PolarityKey,Interval)]
      rstuff   = if Set.member catAttr polarityAttrs
                    then -- cat is considered global to the whole tree,
                         -- but to be robust, we grab it from the top feature
                         detectOrBust 1 catAttr rup rup ++ rstuffLite
                    else rstuffLite
      substuff :: [(PolarityKey,Interval)]
      substuff = let tops = map mkFeatStruct (substTops te)
                     detect :: PolarityAttr -> [(PolarityKey,Interval)]
                     detect v = concat $ zipWith (detectOrBust (-1) v) tops tops
                 in concatMap detect $ Set.toList polarityAttrs
      -- substs and roots
      pols  = case ttype te of
                Initial -> substuff ++ rstuff
                _       -> substuff
      --
      oldfm = tpolarities te
  in te { tpolarities = foldr addPol oldfm pols }


detectPolarity :: Int          -- ^ polarity to assign
               -> PolarityAttr -- ^ attribute to look for
               -> FeatStruct GeniVal -- ^ feature structure to filter on ('RestrictedPolarityAttr' only)
               -> FeatStruct GeniVal -- ^ feature structure to get value from
               -> PolarityDetectionResult
detectPolarity i (RestrictedPolarityAttr cat att) filterFl fl =
  case Map.lookup __cat__ filterFl of
    Nothing -> PD_UserError $ "[polarities] No category " ++ cat ++ " in:" ++ showFeatStruct filterFl
    Just v -> if isJust (unify [mkGConst cat []] [v])
              then detectPolarity i (SimplePolarityAttr att) emptyFeatStruct fl
              else PD_Nothing
detectPolarity i (SimplePolarityAttr att) _ fl =
  case Map.lookup att fl of
    Nothing -> PD_Unconstrained (withZero att)
    Just v  -> case gConstraints v of
             Just [x] -> PD_Just [ (PolarityKeyAv att x, ival i) ]       -- singleton
             Just xs  -> PD_Just $ map (withZero . PolarityKeyAv att) xs   -- interval if ambiguous
             Nothing  -> PD_Unconstrained (withZero att)
 where
   withZero x = (x, toZero i)

toZero :: Int -> Interval
toZero x | x < 0     = (x, 0)
         | otherwise = (0, x)

substNodes :: TagElem -> [GNode GeniVal]
substNodes t = [ gn | gn <- (flatten.ttree) t, gtype gn == Subs ]

substTops :: TagElem -> [Flist GeniVal]
substTops = map gup . substNodes

type SemMap = Map.Map Pred [TagElem]
type PolMap = Map.Map PolarityKey Interval

-- | Adds a new polarity item to a 'PolMap'.  If there already is a polarity
--  for that item, it is summed with the new polarity.
addPol :: (PolarityKey,Interval) -> PolMap -> PolMap
addPol (p,c) m = Map.insertWith (!+!) p c m

-- | Ensures that all states and transitions in the polarity automaton
--   are unique.  This is a slight optimisation so that we don't have to
--   repeatedly check the automaton for state uniqueness during its
--   construction, but it is essential that this check be done after
--   construction
nubAut :: (Ord ab, Ord st) => NFA st ab -> NFA st ab
nubAut aut =
  aut {
      transitions = Map.map (\e -> Map.map nub e) (transitions aut)
  }

__cat__, __idx__  :: String
__cat__  = "cat"
__idx__  = "idx"


-- | Careful, this completely ignores any user errors
pdJusts :: [PolarityDetectionResult] -> [(PolarityKey,Interval)]
pdJusts = concatMap helper
 where helper (PD_Just x) = x
       helper _           = []
