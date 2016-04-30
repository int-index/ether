{-# LANGUAGE ConstraintKinds #-}

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
    -- * Handle functions
    , handle
    , handleT
    ) where

import qualified Control.Monad.Ether.Except as A

-- | See 'Control.Monad.Ether.Except.MonadExcept'.
type MonadExcept e = A.MonadExcept e e

-- | See 'Control.Monad.Ether.Except.throw'.
throw :: forall e m a . MonadExcept e m => e -> m a
throw = A.throw @e

-- | See 'Control.Monad.Ether.Except.catch'.
catch :: forall e m a . MonadExcept e m => m a -> (e -> m a) -> m a
catch = A.catch @e

-- | See 'Control.Monad.Ether.Except.Except'.
type Except e = A.Except e e

-- | See 'Control.Monad.Ether.Except.runExcept'.
runExcept :: Except e a -> Either e a
runExcept = A.runExcept

-- | See 'Control.Monad.Ether.Except.ExceptT'.
type ExceptT e = A.ExceptT e e

-- | See 'Control.Monad.Ether.Except.exceptT'.
exceptT :: m (Either e a) -> ExceptT e m a
exceptT = A.exceptT

-- | See 'Control.Monad.Ether.Except.runExceptT'.
runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT = A.runExceptT

-- | See 'Control.Monad.Ether.Except.handle'.
handle :: (e -> a) -> Except e a -> a
handle = A.handle

-- | See 'Control.Monad.Ether.Except.handleT'.
handleT :: Functor m => (e -> a) -> ExceptT e m a -> m a
handleT = A.handleT
