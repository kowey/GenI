{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- This module is public domain as far as I'm concerned

module NLP.GenI.ErrorIO where

import Control.Monad.Trans.Error
import Data.Text ( Text )
import qualified Data.Text as T

type ErrorIO = ErrorT Text IO

instance Error Text where
    strMsg = T.pack

liftEither :: (Error e, Monad m) => Either e a -> ErrorT e m a
liftEither (Left e)  = throwError e
liftEither (Right x) = return x
