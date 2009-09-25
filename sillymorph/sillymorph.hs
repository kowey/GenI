-- Sillymorph morphological realiser
-- Copyright (C) 2009 Eric Kow
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
--
-- NB: I'm happy to BSD-license this, but GenI is GPLed

{-# LANGUAGE DeriveDataTypeable #-}

import Data.Maybe ( isJust )
import Data.Version ( showVersion )
import System.Console.CmdArgs
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.JSON
import Text.JSON.Pretty hiding ( Mode, mode, text )

import Paths_sillymorph ( version )

import NLP.GenI.Btypes
import NLP.GenI.Morphology
import NLP.GenI.GeniParsers
import qualified System.IO.UTF8 as UTF8

type MorphLexicon = [(String, String, Flist)]

-- ----------------------------------------------------------------------
-- args
-- ----------------------------------------------------------------------

data SillyMorph = SillyMorph { morphLexicon :: FilePath
                             }
 deriving (Show, Data, Typeable)

sillymorph :: Mode SillyMorph
sillymorph = mode $ SillyMorph { morphLexicon = def &= text "Morphological lexicon FILE" }

-- ----------------------------------------------------------------------
-- main
-- ----------------------------------------------------------------------

main :: IO ()
main =
 do config    <- cmdArgs ("sillymorph " ++ showVersion version) [sillymorph]
    morphLex  <- either (fail.show) return
                 =<< parseFromFile parseMorphLexicon (morphLexicon config)
    lemSentences <- either fail return =<< (resultToEither . decode) `fmap` UTF8.getContents
    let _ = lemSentences :: [LemmaPlusSentence]
    let results = map (inflectSentence morphLex) lemSentences
    UTF8.putStrLn . prettyShowJSON $ results

-- ----------------------------------------------------------------------
-- morph realiser
-- ----------------------------------------------------------------------

inflectSentence :: MorphLexicon -> LemmaPlusSentence -> [String]
inflectSentence mlex = map unwords . mapM (inflectWord mlex)

inflectWord :: MorphLexicon -> LemmaPlus -> [String]
inflectWord mlex (LemmaPlus lem fs) =
 case candidates of
   [] -> [lem ++ "-"] -- ^ convention to indicate out-of-vocab item
   xs -> xs
 where
  candidates = [ word | (word, mLem, mFs) <- mlex, lem == mLem, isJust $ fs `unifyFeat` mFs ]

-- ----------------------------------------------------------------------
-- parsers
-- ----------------------------------------------------------------------

parseMorphLexicon :: Parser [MorphLexEntry]
parseMorphLexicon = tillEof $ many morphLexiconEntry

morphLexiconEntry :: Parser (String, String, Flist)
morphLexiconEntry =
 do inflected <- try stringLiteral <|> geniWord
    whiteSpace
    lemma     <-  try stringLiteral <|> geniWord
    whiteSpace
    feats     <- geniFeats
    return (inflected, lemma, feats)

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser geniLanguageDef

whiteSpace :: CharParser () ()
whiteSpace = P.whiteSpace lexer

stringLiteral :: CharParser () String
stringLiteral = P.stringLiteral lexer

-- ----------------------------------------------------------------------
-- odds and ends
-- ----------------------------------------------------------------------

prettyShowJSON :: JSON a => a -> String
prettyShowJSON = render . pp_value . showJSON
