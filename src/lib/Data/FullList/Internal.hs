{-# LANGUAGE DeriveDataTypeable #-}

-- from http://www.haskell.org/haskellwiki/Non-empty_list
-- Safe list functions

module Data.FullList.Internal where

import Data.Binary
import Control.DeepSeq
import Prelude hiding (head, tail, (++))
import qualified Prelude
import Data.Data
import Data.List ( sort, nub )

newtype FullList a = FullList [a]  -- data constructor is not exported!
  deriving (Eq, Ord, Show, Data, Typeable)

fromFL :: FullList a -> [a]
fromFL (FullList x) = x                 -- Injection into general lists

-- The following is an analogue of `maybe'
indeedFL :: [a] -> w -> (FullList a -> w) -> w
indeedFL x on_empty on_full
    | null x = on_empty
    | otherwise = on_full $ FullList x

-- The following are _total_ functions
-- They are guaranteed to be safe, and so we could have used
-- unsafeHead# and unsafeTail# if GHC provides though...

head :: FullList a -> a
head (FullList (x:_)) = x
head (FullList _) = error "NList.head is broken"

tail :: FullList a -> [a]
tail (FullList (_:x)) = x
tail (FullList _) = error "NList.tail is broken"

(++) :: FullList a -> FullList a -> FullList a
(++) x y = FullList ((Prelude.++) (fromFL x) (fromFL y)) -- OK because both already full

sortNub :: (Eq a, Ord a) => FullList a -> FullList a
sortNub xs =
  case (sort . nub . fromFL $ xs) of
   []     -> error "sortNub is broken"
   (y:ys) -> y !: ys

-- Mapping over a non-empty list gives a non-empty list
instance Functor FullList where
    fmap f (FullList x) = FullList (map f x)

-- Adding something to a general list surely gives a non-empty list
infixr 5 !:

class Listable l where
    (!:) :: a -> l a -> FullList a

instance Listable [] where
    (!:) h t = FullList (h:t)

instance Listable FullList where
    (!:) h (FullList t) = FullList (h:t)

{-!
deriving instance NFData a => NFData (FullList a)
deriving instance Binary a => Binary (FullList a)
!-}
-- GENERATED START


instance (NFData a) => NFData (FullList a) where
        rnf (FullList x1) = rnf x1 `seq` ()


instance (Binary a) => Binary (FullList a) where
        put (FullList x1) = put x1
        get
          = do x1 <- get
               return (FullList x1)
-- GENERATED STOP
