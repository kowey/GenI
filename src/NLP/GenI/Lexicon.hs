-- GenI surface realiser
-- Copyright (C) 2005-2009 Carlos Areces and Eric Kow
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

-- | Lexical entries
--
--   As a factorisation technique, LTAG grammars are commonly separated into
--   tree schemata (see 'NLP.GenI.TreeSchema') and lexical entries.  The
--   grammar is what you get by “anchoring” each lexical entry to the relevant
--   tree schemata.
module NLP.GenI.Lexicon (
   Lexicon,
   -- anything but the constructor
   LexEntry, mkLexEntry, mkFullLexEntry,
   iword , ifamname, iparams, iinterface , ifilters, iequations, isemantics, isempols,
   -- * Converting between lexical/normal semantics
   PolValue, fromLexSem, fromLexLiteral,
) where

import NLP.GenI.Lexicon.Internal
