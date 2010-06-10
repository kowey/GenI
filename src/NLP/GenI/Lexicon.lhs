% GenI surface realiser
% Copyright (C) 2005-2009 Carlos Areces and Eric Kow
%
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation; either version 2
% of the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

\chapter{Lexicon}
\label{cha:Lexicon}

\ignore{
\begin{code}
{-# LANGUAGE TemplateHaskell #-}
module NLP.GenI.Lexicon (
   Lexicon, ILexEntry(..), emptyLE,
) where

-- import Debug.Trace -- for test stuff
import Data.Generics (Data)
import Data.Typeable (Typeable)
import qualified Data.Map as Map

import Data.Generics.PlateDirect

import NLP.GenI.FeatureStructures
import NLP.GenI.GeniVal
import NLP.GenI.Semantics
import NLP.GenI.PolarityTypes (SemPols)
import NLP.GenI.TreeSchemata

import Control.Parallel.Strategies
import Data.DeriveTH

--instance Show (IO()) where
--  show _ = ""
\end{code}
}

Every lexical entry is associated with a single family.

\begin{code}
-- | A lexicon maps semantic predicates to lexical entries.
type Lexicon = Map.Map String [ILexEntry]
data ILexEntry = ILE
    { -- normally just a singleton, useful for merging synonyms
      iword       :: [String]
    , ifamname    :: String
    , iparams     :: [GeniVal]
    , iinterface  :: Flist GeniVal
    , ifilters    :: Flist GeniVal
    , iequations  :: Flist GeniVal
    , iptype      :: Ptype
    , isemantics  :: Sem
    , isempols    :: [SemPols] }
  deriving (Show, Eq, Data, Typeable)

emptyLE :: ILexEntry
emptyLE = ILE { iword = [],
                ifamname = "",
                iparams = [],
                iinterface   = [],
                ifilters = [],
                iptype = Unspecified,
                isemantics = [],
                iequations = [],
                isempols   = [] }
\end{code}

\ignore{
\begin{code}
instance Biplate ILexEntry GeniVal where
  biplate (ILE x1 x2 zps zint zfilts zeq x3 zsem x4) =
    plate ILE |- x1 |- x2
              ||* zps
              ||+ zint
              ||+ zfilts
              ||+ zeq  |- x3
              ||+ zsem |- x4

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

-- NFData derivations
$( derive makeNFData ''ILexEntry )

\end{code}
}
