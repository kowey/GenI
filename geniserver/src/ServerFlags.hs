{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-
geniserver
Copyright (C) 2011 Eric Kow

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

module ServerFlags where

import Data.List ( nubBy )
import NLP.GenI.Configuration

myOptions :: [OptDescr Flag]
myOptions = optionsForStandardGenI ++ optionsForServer

optionsForServer :: [OptDescr Flag]
optionsForServer =
  [ Option [] ["port"] (reqArg PortFlg read "INT")
      "port to listen on"
  ]

data PortFlg = PortFlg Int deriving (Eq, Show, Typeable)

optionsForRequest :: [OptDescr Flag]
optionsForRequest=
  optionsForBuilder ++ optionsForOptimisation
