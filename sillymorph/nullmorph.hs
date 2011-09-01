{-# LANGUAGE DeriveDataTypeable, ViewPatterns, PatternGuards, OverloadedStrings #-}

import Control.Applicative ( pure )
import Data.Version ( showVersion )
import System.Console.CmdArgs
import System.IO
import Text.JSON
import Text.JSON.Pretty hiding ( Mode, mode, text )
import Paths_sillymorph ( version )
import Prelude hiding ( reverse )
import qualified Prelude

import NLP.GenI.Morphology
import qualified System.IO.UTF8 as UTF8

-- ----------------------------------------------------------------------
-- args
-- ----------------------------------------------------------------------

data NullMorph = NullMorph
   { reverse :: Bool
   }
 deriving (Show, Data, Typeable)

nullmorph :: NullMorph
nullmorph = modes
            [ NullMorph  { reverse  = False
                         }
            ] &= program "nullmorph"
              &= help ("nullmorph " ++ showVersion version)

-- ----------------------------------------------------------------------
-- main
-- ----------------------------------------------------------------------

main :: IO ()
main = do
  config  <- cmdArgs nullmorph 
  inputs  <- either fail return =<< (resultToEither . decode) `fmap` UTF8.getContents
  let _ = inputs :: [LemmaPlusSentence]
      expansions = map (expandSentence config) inputs
  putStrLn . prettyShowJSON $ expansions 

-- ----------------------------------------------------------------------
-- morph realisation 
-- ----------------------------------------------------------------------

expandSentence :: NullMorph -> LemmaPlusSentence -> [String]
expandSentence config s = unwords `fmap` mapM (expandWord config) s

expandWord :: NullMorph -> LemmaPlus -> [String]
expandWord config w = pure $ if reverse config
                                then Prelude.reverse lemma
                                else lemma
 where
  lemma = lpLemma w

-- ----------------------------------------------------------------------
-- odds and ends
-- ----------------------------------------------------------------------

prettyShowJSON :: JSON a => a -> String
prettyShowJSON = render . pp_value . showJSON
