{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Yage.Core.Application.Exception
    ( ApplicationException(..), InternalException(..)
    , module Ex
    ) where

import           Control.Monad.Exception as Ex



data ApplicationException
    = ApplicationException
    | InitException
    deriving (Show, Typeable)

data InternalException
    = InternalException SomeException
    | WindowCreationException
    | InvalidWindowHandleException
    | IOException SomeException
    | GLFWException SomeException
    deriving (Show, Typeable)


instance Exception ApplicationException
instance Exception InternalException

-- ApplicationException is parent of InternalException
instance Throws InternalException (Caught ApplicationException l)
