-- GenI surface realiser
-- Copyright (C) 2005 Carlos Areces and Eric Kow
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

{-# LANGUAGE RankNTypes #-}
module NLP.GenI.BuilderGui where

import Graphics.UI.WX

import qualified NLP.GenI.Builder as B
import NLP.GenI (ProgState, GeniResult)
import NLP.GenI.LexicalSelection ( CustomSem )
import NLP.GenI.Statistics (Statistics)

-- | Once upon a time, GenI had two very different algorithms for tree assembly,
--   a CKY/Early style one using packed trees; and the traditional “simple“
--   algorithm that remains in 2012-GenI.  Diffrent algorithms may use different
--   chart items, and different debugger visualisations, hence this data structure
--
--   I hope one day to make a move towards implementing the fancier algorithms,
--   which is why I'm keeping around this infrastructure
data BuilderGui = BuilderGui
    { -- | A 'resultsPnl' returns results, statistics and layouts for
      --   a panel showing detailed results (eg. with trees and what not)
      --   and one showing a summary of the results
      resultsPnl  :: forall a sem
                   . ProgState
                  -> CustomSem sem
                  -> Window a -- parent
                  -> sem
                  -> IO ([GeniResult],Statistics,Layout,Layout)
    -- | Just a sentence summary tab (small part of the resultsPnl)
    , summaryPnl :: forall a
                  . ProgState
                 -> Window a -- parent
                 -> [GeniResult]
                 -> Statistics
                 -> IO Layout
    , debuggerPnl :: forall a
                   . ProgState
                  -> Window a -- parent
                  -> B.Input
                  -> String   -- name of the builder algorithm
                  -> ([GeniResult] -> Statistics -> IO ())
                  -- what to do when we get results
                  -> IO Layout
    }
