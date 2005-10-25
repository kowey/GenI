% GenI surface realiser
% Copyright (C) 2005 Carlos Areces and Eric Kow
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

\chapter{GeniParsers}
\label{cha:GeniParsers}

GeniParsers parses everything that GenI knows how to read natively.  For
now, we use Happy for most things, and Parsec for the XMG lexicons 
(via LORIA's in-house TSNLP testing format).

But Parsec is so full of goodness that I hope eventually to rewrite the
other parsers currently in MParser using Parsec as well.

\ignore{
\begin{code}
module GeniParsers ( 
  -- test suite stuff
  parseTSem, parseTSuite,
  geniTestSuite,
  -- macros 
  parseMac,
  -- lexicons
  parseLex, parseMorph, gdeLexicon,
  -- polarity stuff
  parsePol,
  -- parser error status
  E(..), 
  -- tokens used
  Token(..),
) where

import qualified Lex2
import ParserLib
import Mparser
import GdeParser(gdeLexicon)
-- import TsnlpParser 

import Btypes
import Bfuncs (sortSem)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec.Token (TokenParser, 
    LanguageDef(..), makeTokenParser)
import qualified Text.ParserCombinators.Parsec.Token as P
\end{code}
}

\section{Most parsers}

\begin{code}
parseMac    = mParser.(Lex2.lexer Lex2.scMac)
parseLex    = lexParser.(Lex2.lexer 0) --scLex
parseTSem   = targetSemParser.(Lex2.lexer 0) --scTSem
parseTSuite = testSuiteParser.(Lex2.lexer 0) --scTSuite
parsePol    = polParser.(Lex2.lexer 0) --scPol
parseMorph  = morphParser.(Lex2.lexer 0) --scMorph
\end{code}

\ignore{
\begin{code}
\end{code}
}

\section{Test suites}

The test suite format consists of arbitrarily many test cases:

\begin{code}
type SemRes   = ( Sem, [AvPair] ) 
type TestCase = ( String    -- name
                , SemRes    -- semantics / restrictors
                , [String]) -- sentences

geniTestSuite :: Parser [TestCase]
geniTestSuite = many geniTestCase 
\end{code}

A test case is composed of a semantics and its restrictors, followed
by any number of sentences.  Notes:
\begin{itemize}
\item Restrictors are represented as feature structures.  For more
      details about restrictors, see \fnref{detectRestrictors}.
\item Each sentence in the test suite may be optionally preceded by the
      keyword 'sentence'.  We ought to eventually force the use of this
      keyword.
\end{itemize}

\begin{code}
geniTestCase :: Parser TestCase
geniTestCase =
  do name <- identifier <?> "a test case name"
     keyword "semantics" 
     semantics   <- squares geniSemantics 
     restrictors <- option [] resParser 
     sentences   <- many sentenceParser 
     return (name, (semantics,restrictors), sentences)
  where
    resParser :: Parser [AvPair]
    resParser =  
      do keyword "restrictors"
         geniFeats
    --
    sentenceParser :: Parser String
    sentenceParser = 
      do optional (keyword "sentence")
         w <- squares (sepBy identifier whiteSpace<?> "a sentence") 
         return (unwords w)
\end{code}

\section{Generic GenI stuff}

\subsection{Lexer}

Some preliminaries about GenI formats in general - comments start with 
\verb!%!  There is also the option of using \verb'/* */' for embedded
comments.  

\begin{code}
lexer  = makeTokenParser 
         (emptyDef
         { commentLine = "%"
         , commentStart = "/*"
         , commentEnd = "*/"
         })

whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
natural   = P.natural lexer
squares   = P.squares lexer
symbol    = P.symbol  lexer
comma     = P.comma   lexer
braces    = P.braces  lexer
semi      = P.semi    lexer
colon     = P.colon   lexer
parens    = P.parens  lexer
\end{code}

\subsection{Keyword}

A key is nothing simpler than the keyword, followed by a colon.
We factor this into a seperate function to account for whitespace.

\begin{code}
keyword :: String -> Parser ()
keyword k = 
  do let helper = do { symbol k   ; whiteSpace 
                     ; colon ; whiteSpace }
     helper <?> k ++ ":"
\end{code}

\subsection{Feature structures}

Feature structures take the form  \verb!val : att! with only
whitespace to separate each attval pair.  See \fnref{geniValue} for
details about what the values look like.

\begin{code}
geniFeats :: Parser Flist
geniFeats = option [] $ squares $ sepBy geniAttVal whiteSpace 

geniAttVal :: Parser AvPair
geniAttVal = do
  att <- identifier <?> "an attribute"; colon 
  whiteSpace
  val <- geniValue <?> "a GenI value"
  whiteSpace
  return (att, val)
\end{code}

\subsection{Semantics}

A semantics is simply a list of literals. 

\begin{code}
geniSemantics :: Parser Sem
geniSemantics = many (geniLiteral <?> "a literal")
\end{code}

A literal can take one of two forms:
\begin{verbatim}
  handle:predicate(arguments)
         predicate(arguments)
\end{verbatim}

The arguments are space-delimited.  Not providing a handle is
equivalent to providing an anonymous one.

\begin{code}
geniLiteral :: Parser Pred
geniLiteral =  
  do handle    <- option GAnon (handleParser <?> "a handle")      
     predicate <- identifier <?> "a predicate"
     params    <- parens (many geniValue) <?> "some parameters"
     --
     return (handle, predicate, params)
  where handleParser =  
          do { h <- geniValue; whiteSpace; colon; return h } 
\end{code}

\subsection{Miscellaneous}

\fnlabel{geniValue} is recognised both in feature structures and in the 
GenI semantics.

\begin{enumerate}
\item As of geni 0.8, variables are prefixed with a question
      mark.
\item The underscore, \verb!_!, and \verb!?_! are treated as anonymous
      variables.
\item Atomic disjunctions are seperated with a pipe, \verb!|!.  Only
      constants may be separated by atomic disjunction
\item Anything else is just a regular only constant
\end{enumerate}

\begin{code}
geniValue :: Parser GeniVal 
geniValue =   (constants  <?> "a constant or atomic disjunction")
          <|> (variable   <?> "a variable")
          <|> (anonymous  <?> "_ or ?_")
  where 
    question = "?"
    --
    constants :: Parser GeniVal 
    constants = 
      do c <- sepBy1 identifier (symbol "|") 
         return (GConst c)
    variable :: Parser GeniVal
    variable = 
      do symbol question 
         v <- identifier 
         return (GVar v)
    anonymous :: Parser GeniVal
    anonymous = 
      do optional $ symbol question 
         symbol "_"
         return GAnon
\end{code}


