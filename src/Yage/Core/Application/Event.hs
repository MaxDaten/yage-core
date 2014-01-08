{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Yage.Core.Application.Event
    ( module Yage.Core.Application.Event
    , module Event
    ) where

import           Linear

import           Data.Monoid

import           Control.Concurrent.STM         (TVar, modifyTVar', atomically)

import           Yage.Core.Application.Internal.Event as Event


--data InternalEventController
