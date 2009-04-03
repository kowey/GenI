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

-- TODO: use somebody else's test framework... do not let this grow into a
-- custom monstrosity.

module NLP.GenI.Test where

import Test.QuickCheck ( quickCheck )
import NLP.GenI.Btypes

runTests :: IO ()
runTests =
 do putStrLn $ header "unification"
    putStrLn "unification is symmetrical"         >> quickCheck prop_unify_sym
    putStrLn "everything unifies with underscore" >> quickCheck prop_unify_anon
    putStrLn "everything unifies with itself"     >> quickCheck prop_unify_self
 where
  bar = replicate 72 '='
  header x = unlines [bar,x,bar]
