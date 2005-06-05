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
  lexer, 
  -- configuration files
  cParser, giParser, 
  -- test suite stuff
  targetSemParser, testSuiteParser,
  -- macros 
  mParser,           
  -- lexicons
  lexParser, semlexParser, morphParser, filParser, 
  -- polarity stuff
  polParser,
  -- parser error status
  E(..), 
  -- tokens used
  Token(..)
) where

import Mparser (mParser)

import Lex2
import ParserLib
import Cparser
import Mparser
import Lparser 
import Tsparser
