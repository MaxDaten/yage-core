{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ExistentialQuantification  #-}

module Yage.Core.Application.Exception where

import           Control.Monad.Exception
import           Control.Monad.State


data ApplicationException
    = ApplicationException
    | InitException
    | IOException SomeException
    deriving (Show, Typeable)

data InternalException
    = InternalException
    | GLFWException SomeException
    deriving (Show, Typeable)


instance Exception ApplicationException
instance Exception InternalException

-- ApplicationException is parent of InternalException
instance Throws InternalException (Caught ApplicationException l)


-- from: http://hackage.haskell.org/package/control-monad-exception-monadsfd-0.10.3/src/extensions/Control/Monad/Exception/MonadsFD.hs
instance MonadState s m => MonadState s (EMT l m) where
  get = lift get
  put = lift . put
