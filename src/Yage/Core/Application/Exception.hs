{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ExistentialQuantification  #-}

module Yage.Core.Application.Exception where

import           Control.Monad.Exception
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Writer


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
  put = lift . put
  get = lift get

instance MonadReader r m => MonadReader r (EMT l m) where
  ask = lift ask
  local f m = EMT (local f (unEMT m))

instance (Monoid w, MonadWriter w m) => MonadWriter w (EMT l m) where
  tell   = lift . tell
  listen m = EMT $ do
               (res, w) <- listen (unEMT m)
               return (fmap (\x -> (x,w)) res)
  pass m   = EMT $ pass $ do
               a <- unEMT m
               case a of
                 Left  l     -> return (Left l, id)
                 Right (r,f) -> return (Right r, f)
