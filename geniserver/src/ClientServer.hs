{-
GenIClientServer
Copyright (C) 2007 Eric Kow

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

-- Code which is common to both geni client and server

module ClientServer (getPort, ServerInstruction(..), hGetBlock, hPutBlock)
where

import Control.Applicative ( (<$>), (<*>) )
import Data.Char ( isDigit )
import Network
import Text.JSON
import Text.JSON.Pretty ( render, pp_value )
import System.IO
import qualified System.IO.UTF8 as UTF8
import Text.ParserCombinators.Parsec

getPort :: String -> PortID
getPort xs | all isDigit xs = PortNumber . fromIntegral . readInt $ xs
 where
   readInt :: String -> Int
   readInt = read
getPort xs = UnixSocket xs

data ServerInstruction = ServerInstruction
  { gParams    :: [String]
  , gSemantics :: String
  }

instance JSON ServerInstruction where
 readJSON j =
    do jo <- fromJSObject `fmap` readJSON j
       let field x = maybe (fail $ "Could not find: " ++ x) readJSON
                   $ lookup x jo
       ServerInstruction <$> field "params"
                         <*> field "semantics"
 showJSON x =
     JSObject . toJSObject $ [ ("params", showJSONs $ gParams x)
                             , ("semantics", showJSON $ gSemantics x)
                             ]

tween open close =
 do xo  <- char open
    str <- concat `fmap` many (stuff <|> tween open close)
    xc  <- char close
    return $ xo : str ++ [xc]
 where
   stuff = many1 (noneOf [ open, close ])

block = tween '{' '}' <|> tween '[' ']'

hGetBlock :: JSON a => Handle -> IO (Either String a)
hGetBlock h =
 do mp <- parse block "" `fmap` UTF8.hGetContents h
    case mp of
      Left err -> return $ Left (show err)
      Right p  -> return $ resultToEither . decode $ p

-- | See hGetBlock
hPutBlock :: JSON a => Handle -> a -> IO ()
hPutBlock h = UTF8.hPutStr h . render . pp_value . showJSON

instance Eq PortID where
 (==) (UnixSocket s1) (UnixSocket s2) = s1 == s2
 (==) (PortNumber n1) (PortNumber n2) = n1 == n2
 (==) _ _ = False

instance Show PortID where
 show (UnixSocket s) = "UnixSocket " ++ s
 show (PortNumber n) = "PortNumber " ++ show n
