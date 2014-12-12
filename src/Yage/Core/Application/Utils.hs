{-# LANGUAGE FlexibleContexts           #-}
module Yage.Core.Application.Utils where

import           Yage.Prelude                    ((.), io, IO)
import           Control.Monad.Exception
import           Yage.Core.Application.Types
import           Yage.Core.Application.Exception


-- move to a better location
ioe :: (Throws InternalException l) => IO a -> Application l a
ioe = wrapException IOException . io
