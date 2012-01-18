{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Data.FullList
   (FullList,
    fromFL,
    indeedFL,
    head,
    tail,
    (++),
    Listable (..)
    ) where

import Data.FullList.Internal
import Prelude hiding ( head, tail, (++) )
