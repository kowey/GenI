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

-- | This module provides some very generic, non-GenI specific functions on strings,
--   trees and other miscellaneous odds and ends.  Whenever possible, one should try
--   to replace these functions with versions that are available in the standard
--   libraries, or the Haskell platform ones, or on hackage.

module NLP.GenI.General (
        -- * IO
        ePutStr, ePutStrLn, eFlush,
        -- ** Strict readFile
        readFile',
        lazySlurp,
        -- ** Timeouts
        withTimeout,
        exitTimeout,
        -- * Strings
        dropTillIncluding,
        trim,
        toUpperHead, toLowerHead,
        toAlphaNum,
        -- * Triples
        fst3, snd3, thd3,
        -- * Lists
        equating, comparing,
        map',
        wordsBy,
        boundsCheck,
        isEmptyIntersect,
        groupByFM,
        multiGroupByFM,
        insertToListMap,
        groupAndCount,
        combinations,
        mapMaybeM,
        repList,
        -- * Trees
        mapTree', filterTree,
        treeLeaves, preTerminals,
        repNode, repAllNode, listRepNode, repNodeByNode,
        -- * Intervals
        Interval,
        (!+!), ival, showInterval,
        -- * Bit vectors
        BitVector,
        showBitVector,
       -- * Bugs
        geniBug,
        )
        where

import Control.Monad (liftM)
import Data.Bits (shiftR, (.&.))
import Data.Char (isDigit, isSpace, toUpper, toLower)
import Data.List (intersect, groupBy, group, sort)
import Data.Tree
import System.IO (hPutStrLn, hPutStr, hFlush, stderr)
import qualified Data.Map as Map

-- for timeout
import Control.Concurrent
import Control.Exception
import Data.Dynamic(Typeable, typeOf, TyCon, mkTyCon, mkTyConApp, toDyn)
import Data.Unique
import System.Exit(exitWith, ExitCode(ExitFailure))

-- for non-lazy IO
import System.IO (openFile, IOMode(ReadMode), hFileSize, hGetBuf)
import System.IO.Unsafe (unsafeInterleaveIO)
import Foreign (mallocForeignPtrBytes, withForeignPtr, ForeignPtr, Ptr, peekElemOff, plusPtr, Word8)
import Data.Char (chr)

-- ----------------------------------------------------------------------
-- IO
-- ----------------------------------------------------------------------

-- | putStr on stderr
ePutStr :: String -> IO ()
ePutStr   = hPutStr stderr

ePutStrLn :: String -> IO()
ePutStrLn = hPutStrLn stderr

eFlush :: IO()
eFlush    = hFlush stderr

-- ----------------------------------------------------------------------
-- Strings
-- ----------------------------------------------------------------------

trim :: String -> String
trim = reverse . (dropWhile isSpace) . reverse . (dropWhile isSpace) 

-- | Drop all characters up to and including the one in question
dropTillIncluding :: Char -> String -> String
dropTillIncluding c = drop 1 . (dropWhile (/= c))

-- | Make the first character of a string upper case
toUpperHead :: String -> String
toUpperHead []    = []
toUpperHead (h:t) = (toUpper h):t

-- | Make the first character of a string lower case
toLowerHead :: String -> String
toLowerHead []    = []
toLowerHead(h:t)  = (toLower h):t

-- ----------------------------------------------------------------------
-- Alphanumeric sort
-- ----------------------------------------------------------------------

-- | Intermediary type used for alphanumeric sort
data AlphaNum = A String | N Int deriving Eq

-- we don't derive this, because we want num < alpha
instance Ord AlphaNum where
 compare (A s1)  (A s2) = compare s1 s2
 compare (N s1)  (N s2) = compare s1 s2
 compare (A _)   (N _)  = GT
 compare (N _)   (A _)  = LT

-- | An alphanumeric sort is one where you treat the numbers in the string
--   as actual numbers.  An alphanumeric sort would put x2 before x100,
--   because 2 < 10, wheraeas a naive sort would put it the other way
--   around because the characters 1 < 2.  To sort alphanumerically, just
--   'sortBy (comparing toAlphaNum)'
toAlphaNum :: String -> [AlphaNum]
toAlphaNum = map readOne . groupBy (equating isDigit)
 where
   readOne s
     | all isDigit s = N (read s)
     | otherwise      = A s

-- ----------------------------------------------------------------------
-- Triples
-- ----------------------------------------------------------------------

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

thd3 :: (a,b,c) -> c
thd3 (_,_,x) = x

-- ----------------------------------------------------------------------
-- Lists
-- ----------------------------------------------------------------------

equating :: Eq b => (a -> b) -> (a -> a -> Bool)
equating f a b = f a == f b

comparing :: Ord b => (a -> b) -> (a -> a -> Ordering)
comparing f a b = compare (f a) (f b)

-- | A strict version of 'map'
map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = let a = f x in a `seq` (a:(map' f xs))

-- | A generic version of the Data.List.words
--   TODO: replace by version from split
wordsBy :: (Eq a) => a -> [a] -> [[a]]
wordsBy c xs = filter (/= [c]) $ groupBy (\x y -> x /= c && y /= c) xs

-- | Makes sure that index s is in the bounds of list l.  
--   Surely there must be some more intelligent way to deal with this.
boundsCheck :: Int -> [a] -> Bool
boundsCheck s l = s >= 0 && s < length l

-- | True if the intersection of two lists is empty.
isEmptyIntersect :: (Eq a) => [a] -> [a] -> Bool
isEmptyIntersect a b = null $ intersect a b

-- ----------------------------------------------------------------------
-- Grouping
-- ----------------------------------------------------------------------

-- | Serves the same function as 'Data.List.groupBy'.  It groups together
--   items by some property they have in common. The difference is that the
--   property is used as a key to a Map that you can lookup.
groupByFM :: (Ord b) => (a -> b) -> [a] -> (Map.Map b [a])
groupByFM fn list = 
  let addfn  x acc key = insertToListMap key x acc
      helper acc x = addfn x acc (fn x)
  in foldl helper Map.empty list

-- | Same as 'groupByFM', except that we let an item appear in
--   multiple groups.  The fn extracts the property from the item,
--   and returns multiple results in the form of a list
multiGroupByFM :: (Ord b) => (a -> [b]) -> [a] -> (Map.Map b [a])
multiGroupByFM fn list = 
  let addfn  x acc key = insertToListMap key x acc
      helper acc x = foldl (addfn x) acc (fn x)
  in foldl helper Map.empty list

{-# INLINE insertToListMap #-}
insertToListMap :: (Ord b) => b -> a -> Map.Map b [a] -> Map.Map b [a]
insertToListMap k i m =
  case Map.lookup k m of
  Nothing -> Map.insert k [i] m
  Just p  -> Map.insert k (i:p) m

-- | Convert a list of items into a list of tuples (a,b) where
--   a is an item in the list and b is the number of times a
--   in occurs in the list.
groupAndCount :: (Eq a, Ord a) => [a] -> [(a, Int)]
groupAndCount xs = 
  map (\x -> (head x, length x)) grouped
  where grouped = (group.sort) xs

-- Given a list of lists, return all lists such that one item from each sublist is chosen.
-- If returns the empty list if there are any empty sublists.
combinations :: [[a]] -> [[a]]
combinations = sequence

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM _ [] = return []
mapMaybeM f (x:xs) =
 f x >>=
 (\my -> case my of
          Nothing -> mapMaybeM f xs
	  Just y  -> liftM (y:) (mapMaybeM f xs))

-- | Return the list, modifying only the first matching item.
repList :: (a->Bool) -> (a->a) -> [a] -> [a]
repList _ _ [] = []
repList pr fn (x:xs)
  | pr x = fn x : xs
  | otherwise = x : (repList pr fn xs)

-- ----------------------------------------------------------------------
-- Trees
-- ----------------------------------------------------------------------

-- | Strict version of 'mapTree' (for non-strict, just use fmap)
mapTree' :: (a->b) -> Tree a -> Tree b
mapTree' fn (Node a []) = let b = fn a in b `seq` Node b []
mapTree' fn (Node a l)  = let b = fn a
                              bs = map' (mapTree' fn) l
                          in b `seq` bs `seq` Node b bs

-- | Like 'filter', except on Trees.  Filter might not be a good name, though,
--   because we return a list of nodes, not a tree.
filterTree :: (a->Bool) -> Tree a -> [a]
filterTree fn = (filter fn) . flatten

-- | The leaf nodes of a Tree
treeLeaves :: Tree a -> [a]
treeLeaves (Node n []) = [n]
treeLeaves (Node _ l ) = concatMap treeLeaves l

-- | Return pairs of (parent, terminal)
preTerminals :: Tree a -> [(a,a)]
preTerminals (Node _ []) = []
preTerminals (Node x ks) =
 [ (x,y) | (Node y ys) <- ks, null ys ] ++ concatMap preTerminals ks

-- | 'repNode' @fn filt t@ returns a version of @t@ in which the first
--   node which @filt@ matches is transformed using @fn@.
repNode :: (Tree a -> Tree a) -- ^ replacement function
        -> (Tree a -> Bool)   -- ^ filtering function
        -> Tree a -> Maybe (Tree a)
repNode fn filt t =
 case listRepNode fn filt [t] of
 (_, False)   -> Nothing
 ([t2], True) -> Just t2
 _            -> geniBug "Either repNode or listRepNode are broken"

-- | Like 'repNode' except that it performs the operations on
--   all nodes that match and doesn't care if any nodes match
--   or not
repAllNode :: (Tree a -> Tree a) -> (Tree a -> Bool)
           -> Tree a -> Tree a
repAllNode fn filt n | filt n = fn n
repAllNode fn filt (Node p ks) = Node p $ map (repAllNode fn filt) ks

-- | Like 'repNode' but on a list of tree nodes
listRepNode :: (Tree a -> Tree a) -- ^ replacement function
            -> (Tree a -> Bool)   -- ^ filtering function
            -> [Tree a]           -- ^ nodes
            -> ([Tree a], Bool)
listRepNode _ _ [] = ([], False)
listRepNode fn filt (n:l2) | filt n = (fn n : l2, True)
listRepNode fn filt ((n@(Node a l1)):l2) =
  case listRepNode fn filt l1 of
  (lt1, True) -> ((Node a lt1):l2, True)
  _ -> case listRepNode fn filt l2 of
       (lt2, flag2) -> (n:lt2, flag2)

-- | Replace a node in the tree in-place with another node; keep the
--   children the same.  If the node is not found in the tree, or if
--   there are multiple instances of the node, this is treated as an
--   error.
repNodeByNode :: (a -> Bool) -- ^ which node?
              -> a -> Tree a -> Tree a
repNodeByNode nfilt rep t =
 let tfilt (Node n _) = nfilt n
     replaceFn (Node _ k) = Node rep k
 in case listRepNode replaceFn tfilt [t] of
    ([t2], True) -> t2
    (_ ,  False) -> geniBug "Node not found in repNode"
    _            -> geniBug "Unexpected result in repNode"

-- ----------------------------------------------------------------------
-- Errors
-- ----------------------------------------------------------------------

-- | errors specifically in GenI, which is very likely NOT the user's fault.
geniBug :: String -> a
geniBug s = error $ "Bug in GenI!\n" ++ s ++
                    "\nPlease file a report on http://wiki.loria.fr/wiki/GenI/Complaints" 

-- ----------------------------------------------------------------------
-- Intervals
-- ----------------------------------------------------------------------

type Interval = (Int,Int)

-- | Add two intervals
(!+!) :: Interval -> Interval -> Interval
(!+!) (a1,a2) (b1,b2) = (a1+b1, a2+b2)

-- | 'ival' @x@ builds a trivial interval from 'x' to 'x'
ival :: Int -> Interval
ival i = (i,i)

showInterval :: Interval -> String
showInterval (x,y) =
 let sign i = if i > 0 then "+" else ""
     --
 in if (x==y) 
    then (sign x) ++ (show x) 
    else show (x,y)

-- ----------------------------------------------------------------------
-- Bit vectors
-- ----------------------------------------------------------------------

type BitVector = Integer

-- | displays a bit vector, using a minimum number of bits
showBitVector :: Int -> BitVector -> String
showBitVector min_ 0 = replicate min_ '0'
showBitVector min_ x = showBitVector (min_ - 1) (shiftR x 1) ++ (show $ x .&. 1)

-- ----------------------------------------------------------------------
-- Strict readfile
-- Simon Marlow wrote this code on the Haskell mailing list 2005-08-02.
-- ----------------------------------------------------------------------

-- | Using readFile' can be a good idea if you're dealing with not-so-huge
-- files (i.e. where you don't want lazy evaluation), because it ensures
-- that the handles are closed. No more ``too many open files''
readFile' :: FilePath -> IO String
readFile' f = do
  h <- openFile f ReadMode
  s <- hFileSize h
  fp <- mallocForeignPtrBytes (fromIntegral s)
  len <- withForeignPtr fp $ \buf -> hGetBuf h buf (fromIntegral s)
  lazySlurp fp 0 len

buf_size :: Int
buf_size = 4096 :: Int

lazySlurp :: ForeignPtr Word8 -> Int -> Int -> IO String
lazySlurp fp ix len
  | fp `seq` False = undefined
  | ix >= len = return []
  | otherwise = do
      cs <- unsafeInterleaveIO (lazySlurp fp (ix + buf_size) len)
      ws <- withForeignPtr fp $ \p -> loop (min (len-ix) buf_size - 1)
					((p :: Ptr Word8) `plusPtr` ix) cs
      return ws
 where
  loop :: Int -> Ptr Word8 -> String -> IO String
  loop sublen p acc
    | sublen `seq` p `seq` False = undefined
    | sublen < 0 = return acc
    | otherwise = do
       w <- peekElemOff p sublen
       loop (sublen-1) p (chr (fromIntegral w):acc)

-- ----------------------------------------------------------------------
-- Timeouts
-- ----------------------------------------------------------------------

data TimeOut = TimeOut Unique

timeOutTc :: TyCon
timeOutTc = mkTyCon "TimeOut"

instance Typeable TimeOut where
    typeOf _ = mkTyConApp timeOutTc []

withTimeout :: Integer
            -> IO a -- ^ action to run upon timing out
            -> IO a -- ^ main action to run
            -> IO a
withTimeout secs on_timeout action =
 do parent  <- myThreadId
    i       <- newUnique
    block $ do
      timeout <- forkIO (timeout_thread secs parent i)
      Control.Exception.catchDyn
        ( unblock $ do result <- action
                       killThread timeout
                       return result )
        ( \ex -> case ex of
                 TimeOut u | u == i -> unblock on_timeout
                 _ -> killThread timeout >>= throwDyn ex )
 where
  timeout_thread secs_ parent i =
   do threadDelay $ (fromInteger secs_) * 1000000
      throwTo parent (DynException $ toDyn $ TimeOut i)

-- | Like 'exitFailure', except that we return with a code that we reserve for timing out
exitTimeout :: IO ()
exitTimeout = exitWith $ ExitFailure 2