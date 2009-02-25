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

module ClientServer (hardCodedPort, hGetBeginEnd, hGetNonEmptyLine)
where

import System.IO
import Network

hardCodedPort:: PortID
hardCodedPort = UnixSocket "/tmp/geniserver"
                -- (PortNumber 2035)

-- keep reading a line until we hit a complete task description
-- or run into some error
hGetBeginEnd :: String -- ^ keyword
             -> Handle -> IO (Either String [String])
hGetBeginEnd key h =
 do l <- hGetNonEmptyLine h
    if l == ("begin " ++ key)
       then helper []
       else return $ Left "no begin task"
 where
  helper acc =
   do l <- hGetNonEmptyLine h
      if l == ("end " ++ key)
         then return $ Right (reverse acc)
         else helper (l:acc)

hGetNonEmptyLine :: Handle -> IO String
hGetNonEmptyLine h =
 do l <- hGetLine h
    case l of
      "" -> hGetNonEmptyLine h
      _  -> return l
