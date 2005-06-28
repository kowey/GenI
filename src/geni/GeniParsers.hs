{- GenI surface realiser
Copyright (C) 2005 Carlos Areces and Eric Kow

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
-}

-- This module simply re-exports all functions for GenI parsers.

module GeniParsers ( 
  -- configuration files
  parseConfig, parseIndex,
  -- test suite stuff
  parseTSem, parseTSuite,
  -- macros 
  parseMac,
  -- lexicons
  parseLex, parseMorph, parseFil,
  -- polarity stuff
  parsePol,
  -- parser error status
  E(..), 
  -- tokens used
  Token(..),
) where

import Lex2
import ParserLib
import Cparser
import Mparser
import Lparser 
import Tsparser

parseMac    = mParser.(lexer scMac)
parseConfig = cParser.(lexer 0) --scConfig
parseLex    = lexParser.(lexer 0) --scLex
parseIndex  = giParser.(lexer 0) --scIndex
parseTSem   = targetSemParser.(lexer 0) --scTSem
parseTSuite = testSuiteParser.(lexer 0) --scTSuite
parsePol    = polParser.(lexer 0) --scPol
parseMorph  = morphParser.(lexer 0) --scMorph
parseFil    = filParser.(lexer 0) --scFil