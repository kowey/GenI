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

{-# LANGUAGE OverloadedStrings #-}
module NLP.GenI.Warning.Internal where

import           Data.FullList                   (FullList, fromFL)
import           Data.List
import qualified Data.Map                        as Map
import           Data.Monoid                     (Monoid, mappend, mconcat,
                                                  mempty)
import           Data.Text                       (Text)
import qualified Data.Text                       as T

import           Data.Poset

import           NLP.GenI.General                (histogram)
import           NLP.GenI.GeniVal                (GeniVal)
import           NLP.GenI.LexicalSelection.Types (LexCombineError,
                                                  showLexCombineError)
import           NLP.GenI.Lexicon                (LexEntry (..))
import           NLP.GenI.Pretty
import           NLP.GenI.Semantics              (Literal)
import           NLP.GenI.TreeSchema             (showLexeme)

-- | This exists because we want the 'Monoid' instance, providing a
--   GenI-specific notion of appending which merges instances of the
--   same error
newtype GeniWarnings = GeniWarnings { fromGeniWarnings :: [GeniWarning] }

mkGeniWarnings :: [GeniWarning] -> GeniWarnings
mkGeniWarnings = mconcat . map (\x -> GeniWarnings [x])

instance Monoid GeniWarnings where
  mempty  = GeniWarnings []
  mappend (GeniWarnings g1) (GeniWarnings g2) = GeniWarnings (foldr appendWarning g2 g1)

data GeniWarning = -- | A warning that should be repeated for each lexical entry affected
                   LexWarning [LexEntry] LexWarning
                   -- | A single custom warning
                 | CustomLexWarning Text
                   -- | Literals which did not receive any lexical selection
                 | NoLexSelection         [Literal GeniVal]
                   -- | Warnings from the morphological realiser
                 | MorphWarning           [Text]
  deriving (Eq)


data LexWarning = LexCombineAllSchemataFailed
                | LexCombineOneSchemaFailed   LexCombineError
                | MissingCoanchors            Text Int
  deriving (Eq)

-- | Sort, treating non-comporable items as equal
posort :: Poset a => [a] -> [a]
posort = sortBy (flip fromPosetCmp)
 where
  fromPosetCmp x1 x2 = case posetCmp x1 x2 of
                         Comp o -> o
                         NComp  -> EQ

instance Poset GeniWarning where
 -- 1. LexWarning
 leq (LexWarning _ w1) (LexWarning _ w2)  = leq w1 w2
 leq (LexWarning _ _)  _                  = True
 -- 2. CustomLexWarning
 leq (CustomLexWarning _)  (LexWarning _ _)      = False
 leq (CustomLexWarning w1) (CustomLexWarning w2) = leq w1 w2
 leq (CustomLexWarning _)  _                     = True
 -- 3. NoLexSelection
 leq (NoLexSelection _) (LexWarning _ _)     = False
 leq (NoLexSelection _) (CustomLexWarning _) = False
 leq (NoLexSelection _) (NoLexSelection _)   = True
 leq (NoLexSelection _) _                    = True
 -- 4. MorphWarning
 leq (MorphWarning _)  (LexWarning _ _)     = False
 leq (MorphWarning _)  (CustomLexWarning _) = False
 leq (MorphWarning _)  (NoLexSelection _)   = False
 leq (MorphWarning w1) (MorphWarning w2)    = leq w1 w2

instance Poset LexWarning where
 -- 1. LexCombineOneSchemaFailed
 leq (LexCombineOneSchemaFailed l1) (LexCombineOneSchemaFailed l2)   = leq l1 l2
 leq (LexCombineOneSchemaFailed _)  _                                = True
 -- 2. LexCombineAllSchemataFailed
 leq LexCombineAllSchemataFailed (LexCombineOneSchemaFailed _)       = False
 leq LexCombineAllSchemataFailed  _                                  = True
 -- 3. MissingCoanchors
 leq (MissingCoanchors _ n1) (MissingCoanchors _ n2)                 = leq n1 n2
 leq (MissingCoanchors _ _) (LexCombineOneSchemaFailed _)            = False
 leq (MissingCoanchors _ _) LexCombineAllSchemataFailed              = False

sortWarnings :: GeniWarnings -> GeniWarnings
sortWarnings (GeniWarnings ws) = GeniWarnings (posort ws)

appendWarning :: GeniWarning -> [GeniWarning] -> [GeniWarning]
appendWarning w0 []     = [w0]
appendWarning w0 (w:ws) = case mergeWarning w0 w of
                            Just w1 -> w1 : ws
                            Nothing -> w  : appendWarning w0 ws

mergeWarning :: GeniWarning -> GeniWarning -> Maybe GeniWarning
mergeWarning (LexWarning ls1 w1) (LexWarning ls2 w2) | w1 == w2 = Just (LexWarning (ls1 ++ ls2) w1)
mergeWarning _ _ = Nothing

-- | A warning may be displayed over several lines
showGeniWarning :: GeniWarning -> [Text]
showGeniWarning (NoLexSelection ps) =
    [ "No lexical entries for literals:" <+> T.unwords (map pretty ps) ]
showGeniWarning (CustomLexWarning w) = [w]
showGeniWarning (LexWarning ls wa)  = do -- list monad
    wf <- Map.toList (toWfCount ls)
    return (msg <> ":" <+> prettyCount showWithFam "lemmas" wf <> suffix)
 where
    (msg, suffix) = showLexWarning wa
    showLexWarning LexCombineAllSchemataFailed =
        ("Lexically selected but anchoring failed for *all* instances of", "")
    showLexWarning (LexCombineOneSchemaFailed lc) =
        showLexCombineError lc
    showLexWarning (MissingCoanchors co n) =
        (T.unwords [ "Expected co-anchor", co
                   , "is missing from", T.pack (show n), "schemata"
                   ]
        , "")
    showWithFam (w, f) = showLexeme (fromFL w) <+> parens f
showGeniWarning (MorphWarning ws) = map ("Morph:" <+>) ws

-- word and all families associated with that word
type WordFamilyCount = Map.Map (FullList Text, Text) Int

toWfCount :: [LexEntry] -> WordFamilyCount
toWfCount = histogram . map toWf
 where
   toWf i = (iword i, ifamname i)
