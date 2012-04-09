-- Copyright   : 2012 Eric Kow 
-- License     : BSD3 (NB: GPL still applies due to GenI 0.20.x) 
{-# LANGUAGE StandaloneDeriving #-}

module NLP.GenI.Test.Show where

import NLP.GenI
import NLP.GenI.FeatureStructures
import NLP.GenI.GeniShow
import NLP.GenI.GeniVal
import NLP.GenI.Lexicon
import NLP.GenI.TreeSchemata

instance Show (AvPair GeniVal) where
    show = geniShow

instance Show ILexEntry where


deriving instance Show PathEqLhs
deriving instance Show NodePathEqLhs
deriving instance Show TopBottom
-- deriving instance Show (GNode [GeniVal])
deriving instance Show (AvPair [GeniVal])
deriving instance Show SchemaNode
deriving instance Show SchemaTree

deriving instance Show MorphOutput

deriving instance Show PolarityDetectionResult

instance Show Literal where
instance Show GTestLiteral where
