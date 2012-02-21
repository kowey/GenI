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

{-# LANGUAGE CPP #-}
module NLP.GenI.Main where

import Control.Applicative ((<$>))
import Data.IORef(newIORef)
import Data.Typeable( Typeable )
import Data.Version ( showVersion )
import System.Environment(getArgs, getProgName)

import Paths_GenI ( version )

import NLP.GenI.Geni(emptyProgState)
import NLP.GenI.Console(consoleGeni)
import NLP.GenI.Configuration (treatArgs, optionsForStandardGenI, processInstructions,
                               usage, optionsSections, Params,
                               hasFlagP, BatchDirFlg(..), DisableGuiFlg(..),
                               HelpFlg(..), VersionFlg(..),
                               readGlobalConfig, setLoggers
                              )
import NLP.GenI.Configuration(setFlagP)
import NLP.GenI.Geni( ProgState(..) )

main :: IO ()
main = do
  args  <- getArgs
  confArgs <- forceGuiFlag <$> (processInstructions =<< treatArgs optionsForStandardGenI args)
  mainWithState (emptyProgState confArgs)

mainWithState :: ProgState -> IO ()
mainWithState pst = do
  pname <- getProgName
  maybe (return ()) setLoggers =<< readGlobalConfig
  pstRef <- newIORef pst
  let has :: (Typeable f, Typeable x) => (x -> f) -> Bool
      has = flip hasFlagP (pa pst)
  case () of
   _ | has HelpFlg               -> putStrLn (usage optionsSections pname)
     | has VersionFlg            -> putStrLn (pname ++ " " ++ showVersion version)
     | otherwise                 -> consoleGeni pstRef

forceGuiFlag :: Params -> Params
forceGuiFlag = setFlagP DisableGuiFlg ()
