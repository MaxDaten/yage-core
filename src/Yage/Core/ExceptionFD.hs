{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Yage.Core.ExceptionFD where

import           Control.Monad.State
import           Control.Monad.Exception


instance MonadState s m => MonadState s (EMT l m) where
  get = lift get
  put = lift . put
