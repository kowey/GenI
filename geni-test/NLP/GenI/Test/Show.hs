-- Copyright   : 2012 Eric Kow 
-- License     : BSD3 (NB: GPL still applies due to GenI 0.20.x) 
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module NLP.GenI.Test.Show where

import NLP.GenI
import NLP.GenI.FeatureStructure
import NLP.GenI.GeniShow
import NLP.GenI.GeniVal
import NLP.GenI.LexicalSelection.Types
import NLP.GenI.Lexicon
import NLP.GenI.Morphology.Types
import NLP.GenI.Polarity.Internal
import NLP.GenI.Polarity.Types
import NLP.GenI.Semantics
import NLP.GenI.TreeSchema

instance Show GeniVal where
    show = geniShow

instance Show (AvPair GeniVal) where
    show = geniShow

instance Show LexEntry where
    show = geniShow

deriving instance Show PathEqLhs
deriving instance Show NodePathEqLhs
deriving instance Show TopBottom
-- deriving instance Show (GNode [GeniVal])
deriving instance Show (AvPair [GeniVal])
deriving instance Show SchemaNode
deriving instance Show SchemaTree

deriving instance Show MorphOutput

deriving instance Show PolarityDetectionResult
deriving instance Show PolarityKey

deriving instance Show (Literal GeniVal)
