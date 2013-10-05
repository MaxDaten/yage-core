{-# LANGUAGE FlexibleContexts           #-}
module Yage.Core.Application.Utils where

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Exception
import           Yage.Core.Application.Types
import           Yage.Core.Application.Exception


-- move to a better location
io :: (Throws InternalException l) => IO a -> Application l a
io m = wrapException IOException $ liftIO m
