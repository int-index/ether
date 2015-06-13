{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | See "Control.Monad.Ether.Except".

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
    , mapExceptT
    ) where

import Data.Proxy
import qualified Control.Monad.Ether.Except as Explicit

-- | See 'Control.Monad.Ether.Except.MonadExcept'.
type MonadExcept e = Explicit.MonadExcept e e

-- | See 'Control.Monad.Ether.Except.throw'.
throw :: forall e m a . MonadExcept e m => e -> m a
throw = Explicit.throw (Proxy :: Proxy e)

-- | See 'Control.Monad.Ether.Except.catch'.
catch :: forall e m a . MonadExcept e m => m a -> (e -> m a) -> m a
catch = Explicit.catch (Proxy :: Proxy e)

-- | See 'Control.Monad.Ether.Except.Except'.
type Except e = Explicit.Except e e

-- | See 'Control.Monad.Ether.Except.runExcept'.
runExcept :: Except e a -> Either e a
runExcept = Explicit.runExcept Proxy

-- | See 'Control.Monad.Ether.Except.ExceptT'.
type ExceptT e = Explicit.ExceptT e e

-- | See 'Control.Monad.Ether.Except.exceptT'.
exceptT :: m (Either e a) -> ExceptT e m a
exceptT = Explicit.exceptT Proxy

-- | See 'Control.Monad.Ether.Except.runExceptT'.
runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT = Explicit.runExceptT Proxy

-- | See 'Control.Monad.Ether.Except.mapExceptT'.
-- Note that due to implicit tagging the exception type cannot be changed.
mapExceptT :: (m (Either e a) -> n (Either e b)) -> ExceptT e m a -> ExceptT e n b
mapExceptT = Explicit.mapExceptT Proxy
