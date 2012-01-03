{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
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

import Control.Applicative ((<$>),(<*>))
import Control.DeepSeq
import Data.Maybe ( fromMaybe )

import NLP.GenI.GeniVal ( GeniVal )
import NLP.GenI.FeatureStructures ( Flist, showFlist )
import NLP.GenI.GeniParsers ( geniFeats, CharParser, runParser )
import NLP.GenI.Semantics

import Text.JSON

-- ----------------------------------------------------------------------
-- morph input
-- ----------------------------------------------------------------------

type MorphInputFn = Literal -> Maybe (Flist GeniVal)

-- ----------------------------------------------------------------------
-- morph output
-- ----------------------------------------------------------------------

type MorphRealiser = [LemmaPlusSentence] -> [MorphOutput]

data MorphOutput = MorphOutput { moWarnings     :: [String]
                               , moRealisations :: [String]
                               }
  deriving (Ord, Eq)

instance JSON MorphOutput where
 readJSON j =
   case fromJSObject `fmap` readJSON j of
     Error e -> MorphOutput [] <$> readJSON j
     Ok jo   -> do
       let field x = maybe (fail $ "Could not find: " ++ x) readJSON
                   $ lookup x jo
           warnings = maybe (return []) readJSON (lookup "warnings" jo)
       MorphOutput <$> warnings
                   <*> field "realisations"
 showJSON _ = error "Don't know how to render MorphOutput"

-- | A lemma plus its morphological features
data LemmaPlus = LemmaPlus { lpLemma :: String
                           , lpFeats :: Flist GeniVal }
 deriving (Show, Eq, Ord)

-- | A sentence composed of 'LemmaPlus' instead of plain old words
type LemmaPlusSentence = [LemmaPlus]

instance JSON LemmaPlus where
 readJSON j =
    do jo <- fromJSObject `fmap` readJSON j
       let field x = maybe (fail $ "Could not find: " ++ x) readJSON
                   $ lookup x jo
       LemmaPlus <$> field "lemma"
                 <*> (parsecToJSON "lemma-features" geniFeats =<< field "lemma-features")
 showJSON (LemmaPlus l fs) =
     JSObject . toJSObject $ [ ("lemma", showJSON l)
                             , ("lemma-features", showJSON $ showFlist fs)
                             ]

parsecToJSON :: Monad m => String -> CharParser () b -> String -> m b
parsecToJSON description p str =
 case runParser p () "" str of
   Left  err -> fail $ "Couldn't parse " ++ description ++ " because " ++ show err
   Right res -> return res

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
