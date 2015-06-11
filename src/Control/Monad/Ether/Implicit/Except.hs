{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Ether.Implicit.Except
    (
    -- * MonadExcept class
      MonadExcept
    , throw
    , catch
    -- * The Except monad
    , Except
    , runExcept
    -- * The ExceptT monad transformer
    , ExceptT
    , exceptT
    , runExceptT
    ) where

import Data.Proxy
import qualified Control.Monad.Ether.Except as Explicit

type MonadExcept e = Explicit.MonadExcept e e

throw :: forall e m a . MonadExcept e m => e -> m a
throw = Explicit.throw (Proxy :: Proxy e)

catch :: forall e m a . MonadExcept e m => m a -> (e -> m a) -> m a
catch = Explicit.catch (Proxy :: Proxy e)

type Except  e = Explicit.Except  e e

runExcept :: Except e a -> Either e a
runExcept = Explicit.runExcept Proxy

type ExceptT e = Explicit.ExceptT e e

exceptT :: m (Either e a) -> ExceptT e m a
exceptT = Explicit.exceptT Proxy

runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT = Explicit.runExceptT Proxy
