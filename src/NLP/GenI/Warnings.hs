-- GenI surface realiser
-- Copyright (C) 2011 Eric Kow (on behalf of SRI)
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

module NLP.GenI.Warnings where
 
import Data.List
import qualified Data.Map as Map
import Data.Poset

import NLP.GenI.Lexicon ( ILexEntry(..) )
import NLP.GenI.General ( histogram, showWithCount )
import NLP.GenI.LexicalSelection ( LexCombineError, showLexCombineError )
import NLP.GenI.Semantics ( Literal, showLiteral )
import NLP.GenI.TreeSchemata ( showLexeme )

data GeniWarning = LexWarning [ILexEntry] LexWarning 
                 | NoLexSelection         [Literal]
                 | MorphWarning           [String]
  deriving Eq

data LexWarning = LexCombineAllSchemataFailed
                | LexCombineOneSchemaFailed   LexCombineError
                | MissingCoanchors            String Int
  deriving Eq

-- | Sort, treating non-comporable items as equal
posort :: Poset a => [a] -> [a]
posort = sortBy (flip fromPosetCmp)
 where
  fromPosetCmp x1 x2 = case posetCmp x1 x2 of
                         Comp o -> o
                         NComp  -> EQ

instance Poset GeniWarning where
 leq (LexWarning _ w1) (LexWarning _ w2)  = leq w1 w2
 leq (MorphWarning w1) (MorphWarning w2)  = leq w1 w2
 leq (LexWarning _  _) _                  = True
 leq _ (MorphWarning _)                   = True
 leq _ _                                  = False 

instance Poset LexWarning where
 leq (LexCombineOneSchemaFailed l1) (LexCombineOneSchemaFailed l2)   = leq l1 l2
 leq (LexCombineOneSchemaFailed _)  _                                = True
 leq LexCombineAllSchemataFailed LexCombineAllSchemataFailed         = True
 leq LexCombineAllSchemataFailed (MissingCoanchors _ _)              = True
 leq (MissingCoanchors _ n1) (MissingCoanchors _ n2)                 = leq n1 n2
 leq _ _                                                             = False

instance Show GeniWarning where
  show = intercalate "\n" .  showGeniWarning

sortWarnings :: [GeniWarning] -> [GeniWarning]
sortWarnings = posort

appendWarning :: GeniWarning -> [GeniWarning] -> [GeniWarning]
appendWarning w0 []     = [w0]
appendWarning w0 (w:ws) = case mergeWarning w0 w of
                            Just w1 -> w1 : ws
                            Nothing -> w  : appendWarning w0 ws

mergeWarning :: GeniWarning -> GeniWarning -> Maybe GeniWarning
mergeWarning (LexWarning ls1 w1) (LexWarning ls2 w2) | w1 == w2 = Just (LexWarning (ls1 ++ ls2) w1)
mergeWarning _ _ = Nothing

-- | A warning may be displayed over several lines
showGeniWarning :: GeniWarning -> [String]
showGeniWarning (NoLexSelection ps) = [ "No lexical entries for literals: " ++ unwords (map showLiteral ps) ]
showGeniWarning (LexWarning ls wa)  =
  do -- list monad
     let (msg, suffix) = showLexWarning wa
     wf <- Map.toList (toWfCount ls)
     return (msg ++ ": " ++ showWithCount showWithFam "lemmas" wf ++ suffix)
 where
  showLexWarning lw =
    case lw of
     LexCombineAllSchemataFailed  -> ("Lexically selected but anchoring failed for *all* instances of", "")
     LexCombineOneSchemaFailed lc -> showLexCombineError lc
     MissingCoanchors co n        -> ("Expected co-anchor " ++ co ++ " is missing from " ++ show n ++ " schemata", "")
  showWithFam (w, f) = showLexeme w ++ " (" ++ f ++ ")"
showGeniWarning (MorphWarning ws) = map ("Morph: " ++) ws

-- word and all families associated with that word
type WordFamilyCount = Map.Map ([String],String) Int

toWfCount :: [ILexEntry] -> WordFamilyCount
toWfCount = histogram . map toWf
 where
   toWf i = (iword i, ifamname i)
