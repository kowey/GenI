% GenI surface realiser
% Copyright (C) 2005 Carlos Areces and Eric Kow
%
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation; either version 2
% of the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

\chapter{General}

This module provides some very generic, non-Geni specific functions on strings,
trees and other miscellaneous odds and ends.

\begin{code}
module NLP.GenI.General
where
\end{code}

\ignore{
\begin{code}
import Control.Monad (liftM, liftM2)
import Data.Bits (shiftR, (.&.))
import Data.Char (isSpace, toUpper, toLower)
import Data.List (intersect, groupBy, group, sort, intersperse)
import Data.Tree
import System.IO (hPutStrLn, hPutStr, hFlush, stderr)
import qualified Data.Map as Map

-- for timeout
import Control.Concurrent
import Control.Exception
import Data.Dynamic(Typeable, typeOf, TyCon, mkTyCon, mkTyConApp, toDyn)
import Data.Unique
import System.Exit(exitWith, ExitCode(ExitFailure))
\end{code}
}

\section{IO}

\fnlabel{stderr} PutStr and PutStrLn an stderr
\begin{code}
ePutStr, ePutStrLn :: String -> IO()
ePutStr   = hPutStr stderr
ePutStrLn = hPutStrLn stderr

eFlush :: IO()
eFlush    = hFlush stderr
\end{code}

\section{Strings}

Haskell seems to be missing a string library.  Here are some functions I had to
implement.

\begin{code}
trim :: String -> String
trim = reverse . (dropWhile isSpace) . reverse . (dropWhile isSpace) 
\end{code}

\fnlabel{toUpperHead and toLowerHead} make the first character of a
string upper and lower case, respectively.  

\begin{code}
toUpperHead, toLowerHead :: String -> String
toUpperHead []    = []
toUpperHead (h:t) = (toUpper h):t
toLowerHead []    = []
toLowerHead(h:t)  = (toLower h):t
\end{code}

\section{Three-tuples}

\begin{code}
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

thd3 :: (a,b,c) -> c
thd3 (_,_,x) = x
\end{code}

\section{Lists}

\begin{code}
-- | A forgiving version of tail : if you give it the empty list, it returns the empty list
tail_ :: [a] -> [a]
tail_ [] = []
tail_ (_:t) = t
\end{code}

A generic version of the Data.List.words

\begin{code}
wordsBy :: (Eq a) => a -> [a] -> [[a]]
wordsBy c xs = filter (/= [c]) $ groupBy (\x y -> x /= c && y /= c) xs

-- | Makes sure that index s is in the bounds of list l.  
--   Surely there must be some more intelligent way to deal with this.
boundsCheck :: Int -> [a] -> Bool
boundsCheck s l = s >= 0 && s < length l
\end{code}

\fnlabel{isEmptyIntersect} is true if the intersection of two lists is
empty.

\begin{code}
isEmptyIntersect :: (Eq a) => [a] -> [a] -> Bool
isEmptyIntersect a b = null $ intersect a b
\end{code}

\fnlabel{groupByFM} serves the same function as Data.List.groupBy.  It
groups together items by some property they have in common. The
difference is that the property is used as a key to a Map that you
can lookup.  \texttt{fn} extracts the property from the item.

\begin{code}
groupByFM :: (Ord b) => (a -> b) -> [a] -> (Map.Map b [a])
groupByFM fn list = 
  let addfn  x acc key = Map.insertWith (++) key [x] acc 
      helper x acc     = addfn x acc (fn x)
  in foldr helper Map.empty list 
\end{code}

\fnlabel{multiGroupByFM} is the same as groupByFM, except that we
assume an item can appear in multiple groups.  \texttt{fn} extracts the
property from the item, and returns multiple results in the form of a
list.

\begin{code}
multiGroupByFM :: (Ord b) => (a -> [b]) -> [a] -> (Map.Map b [a])
multiGroupByFM fn list = 
  let addfn  x key acc = Map.insertWith (++) key [x] acc
      helper x acc     = foldr (addfn x) acc (fn x)
  in foldr helper Map.empty list 
\end{code}

\fnlabel{groupAndCount} is a generic list-processing function.
It converts a list of items into a list of tuples (a,b) where 
a is an item in the list and b is the number of times a in occurs 
in the list.

\begin{code}
groupAndCount :: (Eq a, Ord a) => [a] -> [(a, Int)]
groupAndCount xs = 
  map (\x -> (head x, length x)) grouped
  where grouped = (group.sort) xs
\end{code}

\begin{code}
-- Given a list of lists, return all lists such that one item from each sublist is chosen.
-- If returns the empty list if there are any empty sublists.
combinations :: [[a]] -> [[a]]
combinations []     = [[]]
combinations (hs:t) = liftM2 (:) hs (combinations t)
\end{code}

\begin{code}
mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM _ [] = return []
mapMaybeM f (x:xs) =
 f x >>=
 (\my -> case my of
          Nothing -> mapMaybeM f xs
	  Just y  -> liftM (y:) (mapMaybeM f xs))
\end{code}

\begin{code}
-- | Return the list, modifying only the first matching item.
repList :: (a->Bool) -> (a->a) -> [a] -> [a]
repList _ _ [] = []
repList pr fn (x:xs)
  | pr x = fn x : xs
  | otherwise = x : (repList pr fn xs)
\end{code}

\section{Trees}

\fnlabel{mapTree} is like map, except on Trees.  This has to be
tucked away somewhere (i.e. i must be reinventing the wheel)!

\begin{code}
mapTree :: (a->b) -> Tree a -> Tree b
mapTree fn (Node a []) = (Node (fn a) [])
mapTree fn (Node a l)  = (Node (fn a) (map (mapTree fn) l))
\end{code}

\fnlabel{filterTree} is like filter, except on Trees.  Filter 
might not be a good name, though, because we return a list of 
nodes, not a tree.

\begin{code}
filterTree :: (a->Bool) -> Tree a -> [a]
filterTree fn = (filter fn) . flatten
\end{code}

\fnlabel{treeLeaves} returns the leaf nodes of a Tree.

\begin{code}
treeLeaves :: Tree a -> [a]
treeLeaves (Node n []) = [n]
treeLeaves (Node _ l ) = concatMap treeLeaves l
\end{code}

\fnlabel{listRepNode} is a generic tree-walking/editing function.  It
takes a replacement function, a filtering function and a tree.  It
returns the tree, except that the first node for which the filtering
function returns True is transformed with the replacement function.

\begin{code}
listRepNode :: (Tree a -> Tree a) -> (Tree a -> Bool) 
              -> [Tree a] -> ([Tree a], Bool)
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
\end{code}

\section{Errors}

\begin{code}
-- | errors specifically in GenI, which is very likely NOT the user's fault.
geniBug :: String -> a
geniBug s = error $ "Bug in GenI!\n" ++ s ++
                    "\nPlease file a report on http://wiki.loria.fr/wiki/GenI/Complaints" 
\end{code}

\section{Files}

\begin{code}
-- FIXME: should be OS-independant 
slash :: String
slash = "/"
\end{code}

\section{Intervals}

We represent polarities as intervals $(x,y)$, meaning from $x$ to $y$ including
both $x$ and $y$.

\begin{code}
type Interval = (Int,Int)
\end{code}

\fnlabel{(!+!)} adds two intervals

\begin{code}
(!+!) :: Interval -> Interval -> Interval
(!+!) (a1,a2) (b1,b2) = (a1+b1, a2+b2)
\end{code}

\fnlabel{ival} builds a (trivial) interval from $x$ to $x$
\begin{code}
ival :: Int -> Interval
ival i = (i,i)
\end{code}

Hmm... do i really feel like instantiating a datatype for this?

\begin{code}
showInterval :: Interval -> String
showInterval (x,y) =
 let sign i = if i > 0 then "+" else ""
     --
 in if (x==y) 
    then (sign x) ++ (show x) 
    else show (x,y)
\end{code}

\section{Other}

\begin{code}
type BitVector = Integer

-- | displays a bit vector, using a minimum number of bits
showBitVector :: Int -> BitVector -> String
showBitVector min_ 0 = take min_ $ repeat '0'
showBitVector min_ x = showBitVector (min_ - 1) (shiftR x 1) ++ (show $ x .&. 1)
\end{code}

\paragraph{showTable} pretty-prints an ASCII table from a list of items.
More precisely, it builds this from 
\begin{enumerate}
\item \fnparam{header} a list of headers, 
\item \fnparam{items}  a list of items and
\item \fnparam{displayfn} which converts the items to list of pretty-printed strings.
\end{enumerate}
Each item corresponds to a row.  The list returned by \fnparam{displayfn} ought
to be the same length as \fnparam{header}, since each item in the list
corresponds to a column.  Note that this function tries to make the table
pretty by padding each column to be same length as the header 
(so to adjust the size of columns, just pad the header with spaces).

\begin{code}
showTable :: [String] -> [a] -> (a -> [String]) -> String
showTable header items displayfn = 
  let showIt l = concat $ intersperse " | " $ l
      showLine = concat $ intersperse "-+-" $ map linestr header
      resStr r = zipWith pad (displayfn r) header
      -- a list of "-" with the same length as l 
      linestr str2 = map (const '-') str2
      -- pad str to be as long as str2
      pad str str2 = if (diff > 0) then padding ++ str else str
                     where padding = map (const ' ') [1..diff]
                           diff = (length str2) - (length str)   
      --
      headerStr = showIt header ++ "\n" ++ showLine ++ "\n" 
      bodyStr   = concat $ intersperse "\n" $ map (showIt.resStr) items 
  in headerStr ++ bodyStr
\end{code}

\section{Timeouts}

This code is taken directly from HyLoRes.  I have just reformatted it to be a bit more
readable (in my eyes).

\begin{code}
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
\end{code}
