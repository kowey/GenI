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
 
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Poset

import NLP.GenI.Lexicon ( ILexEntry(..) )
import NLP.GenI.General ( histogram )
import NLP.GenI.LexicalSelection ( LexCombineError )
import NLP.GenI.Semantics ( Pred, showPred )
import NLP.GenI.TreeSchemata ( showLexeme )

data GeniWarning = LexWarning [ILexEntry] LexWarning 
                 | NoLexSelection         [Pred]
  deriving Eq

data LexWarning = LexCombineAllSchemataFailed
                | LexCombineOneSchemaFailed   LexCombineError
                | MissingCoanchors            String Int
  deriving Eq

-- | Sort, treating non-comporable items as equal
posort :: Poset a => [a] -> [a]
posort = sortBy fromPosetCmp
 where
  fromPosetCmp x1 x2 = case posetCmp x1 x2 of
                         Comp o -> o
                         NComp  -> EQ

instance Poset GeniWarning where
 leq (LexWarning _ w1) (LexWarning _ w2)  = leq w1 w2
 leq (LexWarning _  _) (NoLexSelection _) = True
 leq _ _                                  = False 

instance Poset LexWarning where
 posetCmp w1 w2 =
   case (compare `on` ranking) w1 w2 of
     EQ -> if w1 == w2 then Comp EQ else NComp
     LT -> Comp LT
     GT -> Comp GT
  where
   ranking LexCombineAllSchemataFailed   = [1]
   ranking (LexCombineOneSchemaFailed _) = [2]
   ranking (MissingCoanchors _ n)        = [3, n]

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
showGeniWarning (NoLexSelection ps) = [ "No lexical entries for literals: " ++ unwords (map showPred ps) ]
showGeniWarning (LexWarning ls wa)  = [ showLexWarning wa ++ ": " ++ showWithCount showWithFam wf | wf <- Map.toList (toWfCount ls) ]
 where
  showLexWarning lw =
    case lw of
     LexCombineAllSchemataFailed  -> "Lexically selected but could not be anchored any members of its family"
     LexCombineOneSchemaFailed lc -> show lc
     MissingCoanchors co n        -> "Expected co-anchor " ++ co ++ " is missing from " ++ show n ++ " schemata"
  showWithFam (w, f) = showLexeme w ++ " (" ++ f ++ ")"

-- word and all families associated with that word
type WordFamilyCount = Map.Map ([String],String) Int

toWfCount :: [ILexEntry] -> WordFamilyCount
toWfCount = histogram . map toWf
 where
   toWf i = (iword i, ifamname i)

showWithCount :: (a -> String) -> (a, Int) -> String
showWithCount f (x, 1) = f x
showWithCount f (x, n) = f x ++ " ×" ++ show n
