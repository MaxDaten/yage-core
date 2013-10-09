{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Yage.Core.Application.Exception
    ( ApplicationException(..), InternalException(..)
    , module Ex
    ) where

import           Yage.Prelude
import           Control.Monad.Exception as Ex
import qualified Graphics.UI.GLFW        as GLFW (Error)



data ApplicationException
    = ApplicationException
    | InitException
    deriving (Show, Typeable)

data InternalException
    = InternalException SomeException
    | WindowCreationException String
    | InvalidWindowHandleException
    | IOException SomeException
    | GLFWException GLFW.Error
    deriving (Show, Typeable)


instance Exception ApplicationException
instance Exception InternalException

-- ApplicationException is parent of InternalException
instance Throws InternalException (Caught ApplicationException l)
