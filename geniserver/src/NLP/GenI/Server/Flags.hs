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

module NLP.GenI.Server.Flags where

import NLP.GenI.Configuration
import NLP.GenI.General ( snd3 )

serverOptions :: [OptDescr Flag]
serverOptions = nubBySwitches (concatMap snd3 serverOptionsSections)

type OptSection = (String,[OptDescr Flag],[String])

serverOptionsSections :: [OptSection]
serverOptionsSections =
 [ ("Core options", optionsForServer, [])
 , ("Input", optionsForInputFiles, [])
 , ("Algorithm",
     (nubBySwitches $ optionsForBuilder ++ optionsForOptimisation),
     [])
 , ("Morphology", optionsForMorphology, [])
 , ("Client parameters", optionsForRequest, ["Subset of parameters the client can pass in"])
 ]

optionsForServer :: [OptDescr Flag]
optionsForServer =
  [ helpOption, verboseOption
  , macrosOption, lexiconOption
  , Option [] ["port"] (reqArg PortFlg read "INT")
      "port to listen on"
  ]

data PortFlg = PortFlg Int deriving (Eq, Show, Typeable)

optionsForRequest :: [OptDescr Flag]
optionsForRequest=
  optionsForBuilder ++ optionsForOptimisation
