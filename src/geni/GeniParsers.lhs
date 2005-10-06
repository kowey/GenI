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
now, we use Happy for most things, and Parsec for the CGM lexicons 
(via LORIA's in-house TSNLP testing format).
But Parsec is so full of goodness that I hope eventually to rewrite the
other parsers currently in MParser using Parsec as well.

\ignore{
\begin{code}
module GeniParsers ( 
  -- test suite stuff
  parseTSem, parseTSuite,
  -- macros 
  parseMac,
  -- lexicons
  parseLex, parseMorph, parseFil, cgmLexicon,
  -- polarity stuff
  parsePol,
  -- parser error status
  E(..), 
  -- tokens used
  Token(..),
) where

import Lex2
import ParserLib
import Mparser
import Btypes
import Bfuncs (sortSem)
import TsnlpParser 
\end{code}
}

\section{Most parsers}

\begin{code}
parseMac    = mParser.(lexer scMac)
parseLex    = lexParser.(lexer 0) --scLex
parseTSem   = targetSemParser.(lexer 0) --scTSem
parseTSuite = testSuiteParser.(lexer 0) --scTSuite
parsePol    = polParser.(lexer 0) --scPol
parseMorph  = morphParser.(lexer 0) --scMorph
parseFil    = filParser.(lexer 0) --scFil
\end{code}

Common Grammar Manifesto (CGM) parsers

\begin{code}
cgmLexicon = tsnlpLexicon
\end{code}
