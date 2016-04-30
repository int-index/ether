{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}

-- | See "Control.Monad.Except".

module Control.Monad.Ether.Except
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
    , handleT
    , handle
    ) where

import GHC.Prim (Proxy#, proxy#)
import Control.Monad.Ether.Except.Class (MonadExcept)
import qualified Control.Monad.Ether.Except.Class as C
import Control.Monad.Trans.Ether.Except hiding (throw, catch)
import Data.Functor.Identity (Identity(..))

-- | Is used within a monadic computation to begin exception processing.
throw :: forall tag e m a . MonadExcept tag e m => e -> m a
throw = C.throw (proxy# :: Proxy# tag)

-- | A handler function to handle previous exceptions and return to
-- normal execution.
catch :: forall tag e m a . MonadExcept tag e m => m a -> (e -> m a) -> m a
catch = C.catch (proxy# :: Proxy# tag)

-- | Runs an 'Except' and handles the exception with the given function.
handle :: forall tag e a . (e -> a) -> Except tag e a -> a
handle h m = runIdentity (handleT @tag h m)

-- | Runs an 'ExceptT' and handles the exception with the given function.
handleT :: forall tag e m a . Functor m => (e -> a) -> ExceptT tag e m a -> m a
handleT h m = fmap (either h id) (runExceptT @tag m)
