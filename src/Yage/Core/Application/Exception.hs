{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Yage.Core.Application.Exception where

import           Control.Monad.Exception
import           Control.Monad.State


data ApplicationException
    = ApplicationException
    | InitException
    | GLFWException SomeException
    deriving (Show, Typeable)


instance Exception ApplicationException


-- from: http://hackage.haskell.org/package/control-monad-exception-monadsfd-0.10.3/src/extensions/Control/Monad/Exception/MonadsFD.hs
instance MonadState s m => MonadState s (EMT l m) where
  get = lift get
  put = lift . put
