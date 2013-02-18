{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
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

module NLP.GenI.Morphology.Types where

import           Control.DeepSeq
import           Data.Text                 (Text)

import           NLP.GenI.FeatureStructure (Flist)
import           NLP.GenI.GeniVal          (GeniVal)
import           NLP.GenI.Semantics

-- ----------------------------------------------------------------------
-- morph input
-- ----------------------------------------------------------------------

type MorphInputFn = Literal GeniVal -> Maybe (Flist GeniVal)

-- ----------------------------------------------------------------------
-- morph output
-- ----------------------------------------------------------------------

type MorphRealiser = [LemmaPlusSentence] -> [MorphOutput]

data MorphOutput = MorphOutput { moWarnings     :: [Text]
                               , moRealisations :: [Text]
                               }
  deriving (Ord, Eq)

-- | A lemma plus its morphological features
data LemmaPlus = LemmaPlus
    { lpLemma :: Text
    , lpFeats :: Flist GeniVal
    }
 deriving (Eq, Ord)

-- | A sentence composed of 'LemmaPlus' instead of plain old words
type LemmaPlusSentence = [LemmaPlus]

{-!
deriving instance NFData MorphOutput
deriving instance NFData LemmaPlus
!-}

-- GENERATED START


instance NFData MorphOutput where
        rnf (MorphOutput x1 x2) = rnf x1 `seq` rnf x2 `seq` ()


instance NFData LemmaPlus where
        rnf (LemmaPlus x1 x2) = rnf x1 `seq` rnf x2 `seq` ()
-- GENERATED STOP
