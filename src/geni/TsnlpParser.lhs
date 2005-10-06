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

\chapter{TSNLP Parser}
\label{cha:TsnlpParser}

The TSNLP parser handles LORIA's in-house lexicon format for use in conjunction
with the common grammar manifesto.  It is implemented in Parsec.  The parser is
currently unused because we are using the GDE lex format instead.

Note: we use huge chunks of code from the GdeParser because they use the same
semantic representation.

\begin{code}
module TsnlpParser(tsnlpLexicon) where
\end{code}

\ignore{
\begin{code}
import Btypes
import GdeParser
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec.Token (TokenParser, 
    LanguageDef(..), makeTokenParser)
import qualified Text.ParserCombinators.Parsec.Token as P
\end{code}
}

\section{Lexer}

Some preliminaries about the lexicon format - comments start with 
\verb!%!.  

\begin{code}
lexer :: P.TokenParser ()
lexer  = makeTokenParser 
         (emptyDef
         { commentLine = "%"
         })

whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
symbol    = P.symbol  lexer
squares   = P.squares lexer
commaSep  = P.commaSep lexer
\end{code}

\section{Scanner}

The lexicon format consists of arbitrarily many entries, 
seperated by whitespace.  

FIXME: Ideally they should be seperated by newlines to
match convention... but it's hard for me to figure out 
how to do this cleanly because I'm using a lexeme parser,
which (unsurprisingly) treats \verb!'\n'! as whitespace.

\begin{code}
tsnlpLexicon :: Parser [ILexEntry] 
tsnlpLexicon = do 
  l <- many tsnlpEntry 
  eof
  return l

tsnlpEntry :: Parser ILexEntry 
tsnlpEntry = do 
  lex <- identifier <?> "lemma"
  identifier <?> "part of speech" -- (ignored) 
  fam    <- identifier <?> "family name"
  feats  <- option [] tsnlpFS <?> "feature structure"
  reltheta <- gdeSem <?> "semantics"
  whiteSpace
  -- pack this all into a lexical entry 
  let (sem,enrich) = relthetaToSem reltheta
      ilex = emptyLE { iword    = lex
                     , ifamname = fam
                     , ifilters = [("family",GConst [fam])]
                     , ipfeat = feats ++ enrich 
                     , isemantics = sem
                     }
  return ilex
\end{code}

Feature structures are surrounded by square brackets and consist of 
a comma-delimited list of attribute-value pairs.

\begin{code}
tsnlpFS :: Parser Flist
tsnlpFS = squares (commaSep tsnlpAttVal)

tsnlpAttVal :: Parser AvPair
tsnlpAttVal = do
  att <- identifier <?> "fs attribute"; symbol "="
  val <- identifier <?> "fs value"
  return (att,GConst [val])
  -- no variables

\end{code}
