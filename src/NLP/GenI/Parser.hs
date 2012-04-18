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

{-# LANGUAGE CPP, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module NLP.GenI.Parser (
  -- * Test suites
  geniTestSuite, geniSemanticInput, geniTestSuiteString,
  geniDerivations,
  toSemInputString,
  -- * Trees
  geniMacros, geniTagElems,
  -- * Lexicon and morph
  geniLexicon, geniMorphInfo,
  -- * Basics
  geniFeats, geniSemantics, geniValue, geniWords,
  -- * Helpers
  geniWord, geniLanguageDef, tillEof,
  --
  parseFromFile, -- UTF-8 version
  module Text.Parsec,
  module Text.Parsec.String,
) where


import Control.Applicative ( (<*>), (<$>), (*>), (<*) )
import Control.Monad (liftM, when)
import Data.Text ( Text )
import Text.Parsec
import Text.Parsec.String hiding ( parseFromFile ) -- TODO: replace with Text.Parsec.Text
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token (TokenParser,
    LanguageDef,
    commentLine, commentStart, commentEnd, opLetter,
    reservedOpNames, reservedNames, identLetter, identStart, 
    makeTokenParser)
import qualified Data.Map  as Map
import qualified Data.Text as T
import qualified Data.Tree as T
import qualified System.IO.UTF8 as UTF8
--import qualified Text.Parsec.Expr  as P
import qualified Text.Parsec.Token as P

import NLP.GenI.FeatureStructure ( Flist, AvPair(..), sortFlist )
import NLP.GenI.General (isGeniIdentLetter)
import NLP.GenI.GeniShow ( GeniShow(..), geniKeyword )
import NLP.GenI.GeniVal ( GeniVal, mkGConst, mkGConstNone, mkGVar, mkGAnon, isAnon )
import NLP.GenI.Lexicon ( fromLexSem, mkFullLexEntry, LexEntry(..) )
import NLP.GenI.Pretty ( above )
import NLP.GenI.Semantics ( Literal(..), Sem, sortSem, LitConstr, SemInput )
import NLP.GenI.Tag (TagElem(..), setTidnums)
import NLP.GenI.TestSuite ( TestCase(..) )
import NLP.GenI.TreeSchema (SchemaTree, Ttree(..), Ptype(..), GNode(..), GType(..) )

-- import BoolExp
import Data.FullList ( FullList, Listable(..) )


-- General notes

-- reserved words
#define SEMANTICS       "semantics"
#define SENTENCE        "sentence"
#define OUTPUT          "output"
#define TRACE           "trace"
#define ANCHOR          "anchor"
#define SUBST           "subst"
#define FOOT            "foot"
#define LEX             "lex"
#define TYPE            "type"
#define ACONSTR         "aconstr"
#define INITIAL         "initial"
#define AUXILIARY       "auxiliary"
#define IDXCONSTRAINTS  "idxconstraints"
#define BEGIN           "begin"
#define END             "end"

-- Lexer

geniLanguageDef :: LanguageDef ()
geniLanguageDef = emptyDef
         { commentLine = "%"
         , commentStart = "/*"
         , commentEnd = "*/"
         , opLetter = oneOf ""
         , reservedOpNames = [""]
         , reservedNames =
             [ SEMANTICS , SENTENCE, OUTPUT, IDXCONSTRAINTS, TRACE
             , ANCHOR , SUBST , FOOT , LEX , TYPE , ACONSTR
             , INITIAL , AUXILIARY
             , BEGIN , END ]
         , identLetter = identStuff
         , identStart  = identStuff
         }
  where identStuff = satisfy isGeniIdentLetter

geniValue :: Parser GeniVal
geniValue =   ((try $ anonymous) <?> "_ or ?_")
          <|> (constants  <?> "a constant or atomic disjunction")
          <|> (variable   <?> "a variable")
  where
    question = "?"
    disjunction = geniAtomicDisjunction
    constants :: Parser GeniVal
    constants = mkGConst <$> disjunction
    variable :: Parser GeniVal
    variable =
      do symbol question
         v <- identifier
         mcs <- option Nothing $ (symbol "/" >> Just `liftM` disjunction)
         return (mkGVar v mcs)
    anonymous :: Parser GeniVal
    anonymous =
      do optional $ symbol question
         symbol "_"
         return mkGAnon

geniAtomicDisjunction :: Parser (FullList Text)
geniAtomicDisjunction = do
    (x:xs) <- atom `sepBy1` (symbol "|")
    return (x !: xs)
  where
    atom = looseIdentifier <|> stringLiteral

geniFancyDisjunction :: Parser [GeniVal]
geniFancyDisjunction = geniValue `sepBy1` symbol ";"

class GeniValLike v where
  geniValueLike :: Parser v

instance GeniValLike GeniVal where
  geniValueLike = geniValue

instance GeniValLike [GeniVal] where
  geniValueLike = geniFancyDisjunction

-- We make no attempt to check for / guarantee uniqueness here
-- because the same sort of format is used for things which are
-- not strictly speaking feature structures
geniFeats :: GeniValLike v => Parser (Flist v)
geniFeats = option [] $ squares $ many geniAttVal

geniAttVal :: GeniValLike v => Parser (AvPair v)
geniAttVal = do
    att <- identifierR <?> "an attribute"; colon
    val <- geniValueLike <?> "a GenI value"
    return $ AvPair att val

geniSemantics :: Parser Sem
geniSemantics =
  do sem <- many (geniLiteral <?> "a literal")
     return (sortSem sem)

geniLiteral :: Parser (Literal GeniVal)
geniLiteral = geniLiteral_ mkGAnon geniValue

geniLiteral_ :: a -> Parser a -> Parser (Literal a)
geniLiteral_ zero gv =
    Literal <$> (option zero handleParser <?> "a handle")
            <*> (gv <?> "a predicate")
            <*> (parens (many gv) <?> "some parameters")
  where
    handleParser = try $ gv <* char ':'

geniSemanticInput :: Parser (Sem,Flist GeniVal,[LitConstr])
geniSemanticInput =
  do keywordSemantics
     (sem,litC) <- liftM unzip $ squares $ many literalAndConstraint
     idxC       <- option [] geniIdxConstraints
     --
     let sem2     = createHandles sem
         semlitC2 = [ (s,c) | (s,c) <- zip sem2 litC, (not.null) c ]
     return (createHandles sem, idxC, semlitC2)
  where
     -- set all anonymous handles to some unique value
     -- this is to simplify checking if a result is
     -- semantically complete
     createHandles :: Sem -> Sem
     createHandles = zipWith setHandle ([1..] :: [Int])
     --
     setHandle i (Literal h pred_ par) =
       let h2 = if isAnon h
                then mkGConstNone ("genihandle" `T.append` T.pack (show i))
                else h
       in Literal h2 pred_ par
     --
     literalAndConstraint :: Parser LitConstr
     literalAndConstraint =
       do l <- geniLiteral
          t <- option [] $ squares $ many identifier
          return (l,t)

-- | The original string representation of the semantics (for gui)
geniSemanticInputString :: Parser Text
geniSemanticInputString = do
    keywordSemantics
    s <- squaresString
    whiteSpace
    optional geniIdxConstraints
    return s

geniIdxConstraints :: Parser (Flist GeniVal)
geniIdxConstraints = keyword IDXCONSTRAINTS >> geniFeats

{-
geniLitConstraints :: Parser (BoolExp T.Text)
geniLitConstraints =
   P.buildExpressionParser table piece
 where
   piece =  (Cond <$> identifier)
       <|> do { string "~"; Not `liftM` geniLitConstraints }
       <|> parens geniLitConstraints
   table = [ [ op "&" And P.AssocLeft ]
           , [ op "|" Or  P.AssocLeft ]
           ]
   op s f assoc = P.Infix (do { string s ; return f }) assoc
-}

squaresString :: Parser Text
squaresString = do
    char '['
    s <- (T.concat <$> many inSq) <|> squaresString
    char ']'
    return $ "[" `T.append` s `T.append` "]"
  where
    inSq :: Parser Text
    inSq = T.pack <$> many1 (noneOf "[]")

-- the output end of things
-- displaying preformatted semantic input

data SemInputString = SemInputString Text (Flist GeniVal)

instance GeniShow SemInputString where
    geniShowText (SemInputString semStr idxC) =
        geniKeyword SEMANTICS semStr `above` r
      where
        r | null idxC = ""
          | otherwise = geniKeyword IDXCONSTRAINTS (geniShowText idxC)

toSemInputString :: SemInput -> Text -> SemInputString
toSemInputString (_,lc,_) s = SemInputString s lc

geniTestSuite :: Parser [TestCase]
geniTestSuite =
  tillEof (many geniTestCase)

-- | Just the String representations of the semantics
--   in the test suite
geniTestSuiteString :: Parser [Text]
geniTestSuiteString =
    tillEof (many geniTestCaseString)

-- | This is only used by the script genimakesuite
geniDerivations :: Parser [TestCaseOutput]
geniDerivations = tillEof $ many geniOutput

geniTestCase :: Parser TestCase
geniTestCase =
     TestCase <$> (option "" (identifier <?> "a test case name"))
              <*> lookAhead geniSemanticInputString
              <*> geniSemanticInput
              <*> many geniSentence

-- note that the keyword is NOT optional
type TestCaseOutput = (Text, Map.Map (Text,Text) [Text])
geniOutput :: Parser TestCaseOutput
geniOutput = do
    ws <- keyword OUTPUT >> squares geniWords
    ds <- Map.fromList <$> many geniTraces
    return (ws, ds)

geniTraces :: Parser ((Text,Text), [Text])
geniTraces = do
    keyword TRACE
    squares $ do
        k1 <- withWhite geniWord
        k2 <- withWhite geniWord
        whiteSpace >> char '!' >> whiteSpace
        traces <- geniWord `sepEndBy1` whiteSpace
        return ((k1,k2), traces)

withWhite :: Parser a -> Parser a
withWhite p = p >>= (\a -> whiteSpace >> return a)

geniSentence :: Parser Text
geniSentence = optional (keyword SENTENCE) >> squares geniWords

geniWords :: Parser Text
geniWords =
    T.unwords <$> (sepEndBy1 geniWord whiteSpace <?> "a sentence")

geniWord :: Parser Text
geniWord = T.pack <$> many1 (noneOf "[]\v\f\t\r\n ")

-- | The original string representation of a test case semantics
--   (for gui)
geniTestCaseString :: Parser Text
geniTestCaseString = do
    option "" (identifier <?> "a test case name")
    geniSemanticInputString <* (many geniSentence >> many geniOutput)

-- ----------------------------------------------------------------------
-- Lexicon
-- ----------------------------------------------------------------------

geniLexicon :: Parser [LexEntry]
geniLexicon = tillEof $ many1 geniLexicalEntry

geniLexicalEntry :: Parser LexEntry
geniLexicalEntry =
  do lemmas  <- geniAtomicDisjunction <?> "a lemma (or disjunction thereof)"
     family  <- identifier <?> "a tree family"
     (pars, interface) <- option ([],[]) $ parens paramsParser
     equations <- option [] $ do keyword "equations"
                                 geniFeats <?> "path equations"
     filters <- option [] $ do keyword "filters"
                               geniFeats
     keywordSemantics
     (sem, pols) <- fromLexSem <$> squares geniLexSemantics
     --
     return (mkFullLexEntry lemmas family pars interface filters equations sem pols)
  where
    paramsParser :: Parser ([GeniVal], Flist GeniVal)
    paramsParser = do
      pars <- many geniValue <?> "some parameters"
      interface <- option [] $ do symbol "!"
                                  many geniAttVal
      return (pars, interface)

geniLexSemantics :: Parser [Literal PolValue]
geniLexSemantics = sortSem <$> many (geniLexLiteral <?> "a literal")

type PolValue = (GeniVal, Int)

geniLexLiteral :: Parser (Literal PolValue)
geniLexLiteral = geniLiteral_ (mkGAnon,0) geniPolValue

geniPolValue :: Parser (GeniVal, Int)
geniPolValue =
  do p <- geniPolarity
     v <- geniValue
     return (v,p)

-- ----------------------------------------------------------------------
-- Tree schemata
-- ----------------------------------------------------------------------

geniMacros :: Parser [SchemaTree]
geniMacros = tillEof $ many geniTreeDef

initType, auxType :: Parser Ptype
initType = do { reserved INITIAL ; return Initial  }
auxType  = do { reserved AUXILIARY ; return Auxiliar }

geniTreeDef :: Parser SchemaTree
geniTreeDef =
  do sourcePos <- getPosition
     family   <- identifier
     tname    <- option "" (colon *> identifier)
     (pars,iface)   <- geniParams
     theTtype  <- (initType <|> auxType)
     theTree  <- geniTree
     -- sanity checks?
     let treeFail x =
          do setPosition sourcePos -- FIXME does not do what I expect
             fail $ "In tree " ++ T.unpack family ++ ":"
                    ++ T.unpack tname ++ " " ++ show sourcePos ++ ": " ++ x
     let theNodes = T.flatten theTree
         numFeet    = length [ x | x <- theNodes, gtype x == Foot ]
         numAnchors = length [ x | x <- theNodes, ganchor x ]
     when (not $ any ganchor theNodes) $
       treeFail "At least one node in an LTAG tree must be an anchor"
     when (numAnchors > 1) $
       treeFail "There can be no more than 1 anchor node in a tree"
     when (numFeet > 1) $
       treeFail "There can be no more than 1 foot node in a tree"
     when (theTtype == Initial && numFeet > 0) $
       treeFail "Initial trees may not have foot nodes"
     --
     psem     <- option Nothing $ do { keywordSemantics; liftM Just (squares geniSemantics) }
     ptrc     <- option [] $ do { keyword TRACE; squares (many identifier) }
     --
     return TT{ params = pars
              , pfamily = family
              , pidname = tname
              , pinterface = sortFlist iface
              , ptype = theTtype
              , tree = theTree
              , ptrace = ptrc
              , psemantics = psem
              }

geniTree :: (Ord v, GeniValLike v) => Parser (T.Tree (GNode v))
geniTree =
  do node <- geniNode
     kids <- option [] (braces $ many geniTree)
             <?> "child nodes"
     -- sanity checks
     let noKidsAllowed t c = when (c node && (not.null $ kids)) $
             fail $ t ++ " nodes may *not* have any children"
     noKidsAllowed "Anchor"       $ ganchor
     noKidsAllowed "Substitution" $ (== Subs) . gtype
     noKidsAllowed "Foot"         $ (== Foot) . gtype
     --
     return (T.Node node kids)

geniNode :: (Ord v, GeniValLike v) => Parser (GNode v)
geniNode = do
    name      <- identifier
    nodeType  <- geniNodeAnnotation
    lex_   <- if nodeType == AnnoLexeme
                 then ((stringLiteral <|> identifier) `sepBy` symbol "|") <?> "some lexemes"
                 else return []
    constr <- case nodeType of
                  AnnoDefault -> adjConstraintParser
                  AnnoAnchor  -> adjConstraintParser
                  _           -> return True
    -- features only obligatory for non-lex nodes
    (top,bot) <- if nodeType == AnnoLexeme
                    then option ([],[]) $ try topbotParser
                    else topbotParser
    return $ GN { gnname   = name
                , gtype    = fromAnnotation nodeType
                , gup      = sortFlist top
                , gdown    = sortFlist bot
                , glexeme  = lex_
                , ganchor  = nodeType == AnnoAnchor
                , gaconstr = constr
                , gorigin  = ""
                }
  where
    adjConstraintParser = option False $ reserved ACONSTR >> char ':' >> symbol "noadj" >> return True
    topbotParser = do
        top <- geniFeats <?> "top features"
        symbol "!"
        bot <- geniFeats <?> "bot features"
        return (top,bot)

-- | Should be purely internal type to help parsing.
--   Injection to 'GType'.
--
--   We don't just use GType directly because the annotations convey
--   subtle distinctions that aren't encoded, particularly between
--   lexemes and anchors
data Annotation = AnnoAnchor
                | AnnoLexeme
                | AnnoSubst
                | AnnoFoot
                | AnnoDefault
  deriving Eq

fromAnnotation :: Annotation -> GType
fromAnnotation AnnoLexeme  = Lex
fromAnnotation AnnoAnchor  = Lex
fromAnnotation AnnoSubst   = Subs
fromAnnotation AnnoFoot    = Foot
fromAnnotation AnnoDefault = Other

geniNodeAnnotation :: Parser Annotation
geniNodeAnnotation =
    (keyword TYPE *> ty)                   <|>
    (reserved ANCHOR >> return AnnoAnchor) <|>
    return AnnoDefault
  where
    ty    = choice [ try (symbol s) >> return t | (s,t) <- table ]
    table =
        [ (ANCHOR, AnnoAnchor)
        , (FOOT,   AnnoFoot)
        , (SUBST,  AnnoSubst)
        , (LEX,    AnnoLexeme)
        ]

-- | This makes it possible to read anchored trees, which may be
--   useful for debugging purposes.
--
--   FIXME: note that this is very rudimentary; we do not set id numbers,
--   parse polarities. You'll have to call
--   some of our helper functions if you want that functionality.
geniTagElems :: Parser [TagElem]
geniTagElems = tillEof $ setTidnums `fmap` many geniTagElem

geniTagElem :: Parser TagElem
geniTagElem = do
    family   <- identifier
    tname    <- option "" $ (colon *> identifier)
    iface    <- (snd `liftM` geniParams) <|> geniFeats
    theType  <- initType <|> auxType
    theTree  <- geniTree
    sem      <- do { keywordSemantics; squares geniSemantics }
    --
    return $ TE { idname      = tname
                , ttreename   = family
                , tinterface  = iface
                , ttype       = theType
                , ttree       = theTree
                , tsemantics  = sem
                , tidnum      = -1 -- provisional id
                , tpolarities = Map.empty
                , tsempols    = []
                , ttrace      = []
                }

-- | 'geniParams' recognises a list of parameters optionally followed by a
--  bang (\verb$!$) and a list of attribute-value pairs.  This whole thing is
--  to wrapped in the parens.
--
--  TODO: deprecate
geniParams :: Parser ([GeniVal], Flist GeniVal)
geniParams = parens $ do
  pars <- many geniValue <?> "some parameters"
  interface <- option [] $ do { symbol "!"; many geniAttVal }
  return (pars, interface)

-- ----------------------------------------------------------------------
-- Morphology
-- ----------------------------------------------------------------------

geniMorphInfo :: Parser [(Text,Flist GeniVal)]
geniMorphInfo = tillEof $ many morphEntry

morphEntry :: Parser (Text,Flist GeniVal)
morphEntry = (,) <$> identifier <*> geniFeats

-- ======================================================================
-- Everything else
-- ======================================================================

-- ----------------------------------------------------------------------
-- Polarities
-- ----------------------------------------------------------------------

-- | 'geniPolarity' associates a numerical value to a polarity symbol,
--  that is, '+' or '-'.
geniPolarity :: Parser Int
geniPolarity = option 0 (plus <|> minus)
  where
    plus  = do { char '+'; return  1   }
    minus = do { char '-'; return (-1) }

-- ----------------------------------------------------------------------
-- keyword
-- ----------------------------------------------------------------------

{-# INLINE keyword #-}
keyword :: Text -> Parser Text
keyword k =
     (try $ do { reserved k; colon; return k }) <?> T.unpack k ++ ":"

{-# INLINE keywordSemantics #-}
keywordSemantics :: Parser Text
keywordSemantics = keyword SEMANTICS

-- ----------------------------------------------------------------------
-- language def helpers
-- ----------------------------------------------------------------------

lexer :: TokenParser ()
lexer  = makeTokenParser geniLanguageDef

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

identifier :: Parser Text
identifier = decode <$> P.identifier lexer

-- stolen from Parsec code (ident)
-- | Like 'identifier' but allows for reserved words too
looseIdentifier :: Parser Text
looseIdentifier =
    decode <$> do { i <- ident ; whiteSpace; return i }
  where
    ident = do
        { c <- identStart geniLanguageDef
        ; cs <- many (identLetter geniLanguageDef)
        ; return (c:cs) } <?> "identifier"

colon :: Parser Text
colon = decode <$> P.colon lexer

stringLiteral :: Parser Text
stringLiteral = decode <$> P.stringLiteral lexer

squares, braces, parens :: Parser a -> Parser a
squares = P.squares lexer
braces  = P.braces  lexer
parens  = P.parens  lexer

reserved :: Text -> Parser Text
reserved s = P.reserved lexer (T.unpack s) >> return s

symbol :: Text -> Parser Text
symbol s = P.symbol lexer (T.unpack s) >> return s

decode :: String -> Text
decode = T.pack

-- ----------------------------------------------------------------------
-- parsec helpers
-- ----------------------------------------------------------------------

-- | identifier, permitting reserved words too
identifierR :: Parser Text
identifierR = decode <$> do
    { c <- P.identStart geniLanguageDef
    ; cs <- many (P.identLetter geniLanguageDef)
    ; return (c:cs)
    } <?> "identifier or reserved word"

tillEof :: Parser a -> Parser a
tillEof p = whiteSpace *> p <* eof

-- stolen from Parsec and adapted to use UTF-8 input
parseFromFile :: Parser a -> SourceName -> IO (Either ParseError a)
parseFromFile p fname
    = do{ input <- UTF8.readFile fname
        ; return (parse p fname input)
        }
