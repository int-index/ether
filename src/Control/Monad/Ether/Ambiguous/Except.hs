{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

-- | See "Control.Monad.Ether.Except".

module Control.Monad.Ether.Ambiguous.Except
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

import GHC.Prim (Proxy#, proxy#)
import Control.Monad.Ether.Except (MonadExcept, ExceptT, Except)
import qualified Control.Monad.Ether.Except as Explicit

-- | See 'Control.Monad.Ether.Except.throw'.
throw :: forall tag e m a . MonadExcept tag e m => e -> m a
throw = Explicit.throw (proxy# :: Proxy# tag)

-- | See 'Control.Monad.Ether.Except.catch'.
catch :: forall tag e m a . MonadExcept tag e m => m a -> (e -> m a) -> m a
catch = Explicit.catch (proxy# :: Proxy# tag)

-- | See 'Control.Monad.Ether.Except.runExcept'.
runExcept :: forall tag e a . Except tag e a -> Either e a
runExcept = Explicit.runExcept proxy#

-- | See 'Control.Monad.Ether.Except.exceptT'.
exceptT :: forall tag e m a . m (Either e a) -> ExceptT tag e m a
exceptT = Explicit.exceptT proxy#

-- | See 'Control.Monad.Ether.Except.runExceptT'.
runExceptT :: forall tag e m a . ExceptT tag e m a -> m (Either e a)
runExceptT = Explicit.runExceptT proxy#

-- | See 'Control.Monad.Ether.Except.handle'.
handle :: forall tag e a . (e -> a) -> Except tag e a -> a
handle = Explicit.handle proxy#

-- | See 'Control.Monad.Ether.Except.handleT'.
handleT :: forall tag e m a . Functor m => (e -> a) -> ExceptT tag e m a -> m a
handleT = Explicit.handleT proxy#
