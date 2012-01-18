-- GenI surface realiser
-- Copyright (C) 2005-2009 Carlos Areces and Eric Kow
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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
module NLP.GenI.Lexicon (
   Lexicon, ILexEntry(..), emptyLE,
) where

-- import Debug.Trace -- for test stuff
import Data.Binary
import Data.Generics (Data)
import Data.Typeable (Typeable)

import NLP.GenI.FeatureStructures
import NLP.GenI.GeniVal
import NLP.GenI.Semantics
import NLP.GenI.Polarity.Types (SemPols)
import NLP.GenI.TreeSchemata

import Control.DeepSeq

--instance Show (IO()) where
--  show _ = ""

type Lexicon = [ILexEntry]
data ILexEntry = ILE
    { -- normally just a singleton, useful for merging synonyms
      iword       :: [String]
    , ifamname    :: String
    , iparams     :: [GeniVal]
    , iinterface  :: Flist GeniVal
    , ifilters    :: Flist GeniVal
    , iequations  :: Flist GeniVal
    , isemantics  :: Sem
    , isempols    :: [SemPols] }
  deriving (Show, Eq, Data, Typeable)

emptyLE :: ILexEntry
emptyLE = ILE { iword = [],
                ifamname = "",
                iparams = [],
                iinterface   = [],
                ifilters = [],
                isemantics = [],
                iequations = [],
                isempols   = [] }

instance DescendGeniVal ILexEntry where
  descendGeniVal s i =
    i { iinterface  = descendGeniVal s (iinterface i)
      , iequations  = descendGeniVal s (iequations i)
      , isemantics  = descendGeniVal s (isemantics i)
      , iparams = descendGeniVal s (iparams i) }

instance Collectable ILexEntry where
  collect l = (collect $ iinterface l) . (collect $ iparams l) .
              (collect $ ifilters l) . (collect $ iequations l) .
              (collect $ isemantics l)

-- ----------------------------------------------------------------------
--
-- ----------------------------------------------------------------------

{-!
deriving instance Binary ILexEntry
deriving instance NFData ILexEntry
!-}

-- GENERATED START

 
instance Binary ILexEntry where
        put (ILE x1 x2 x3 x4 x5 x6 x7 x8)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
               put x8
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               x8 <- get
               return (ILE x1 x2 x3 x4 x5 x6 x7 x8)

 
instance NFData ILexEntry where
        rnf (ILE x1 x2 x3 x4 x5 x6 x7 x8)
          = rnf x1 `seq`
              rnf x2 `seq`
                rnf x3 `seq`
                  rnf x4 `seq` rnf x5 `seq` rnf x6 `seq` rnf x7 `seq` rnf x8 `seq` ()
-- GENERATED STOP
