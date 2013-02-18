{-# LANGUAGE OverloadedStrings #-}
--  GenI surface realiser
--  Copyright (C) 2005-2009 Carlos Areces and Eric Kow
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 2
--  of the License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

module NLP.GenI.TestSuite
where

import           Data.Text         (Text)
import qualified Data.Text         as T

import           NLP.GenI.Control
import           NLP.GenI.General  (maybeQuoteText)
import           NLP.GenI.GeniShow
import           NLP.GenI.Pretty

data TestCase sem = TestCase
    { tcName      :: Text
    , tcSemString :: Text -- ^ for gui
    , tcSem       :: sem
    , tcExpected  :: [Text] -- ^ expected results (for testing)
    , tcParams    :: Maybe Params
    }

instance GeniShow sem => GeniShow (TestCase sem) where
    geniShowText (TestCase { tcName = name
                           , tcExpected = sentences
                           , tcSemString = _semStr
                           , tcSem = sem
                           }) =
        T.unlines $ [ maybeQuoteText name, geniShowText sem ]
            ++ map (geniKeyword "sentence" . squares) sentences

instance GeniShow sem => Pretty (TestCase sem) where
    pretty = geniShowText
