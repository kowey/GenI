-- GenI surface realiser
-- Copyright (C) 2009 Eric Kow
-- Copyright (C) 2005 Carlos Areces
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

{-# LANGUAGE OverlappingInstances, FlexibleInstances, TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module NLP.GenI.GeniVal
  ( -- * GeniVal
    GeniVal, gLabel, gConstraints
  , mkGConst, mkGConstNone, mkGVar, mkGVarNone, mkGAnon
    -- ** queries and manipulation
  , isAnon, singletonVal
  , crushOne
    -- * Unification and subsumption
    --
    -- ** Finalisation
    --
    -- Before you do any unification/subsumption, you should finalise all
    -- the variables in all the objects (a one time alpha-conversion type
    -- thing)
  , finaliseVars, finaliseVarsById, anonymiseSingletons
    -- ** Unification
  , unify, UnificationResult(..), Subst, appendSubst
    -- ** subsumption
  , subsumeOne, allSubsume

    -- * Traversing GeniVal containers
  , DescendGeniVal(..), Collectable(..), Idable(..)
  , replace, replaceList
  ) where

import NLP.GenI.GeniVal.Internal
