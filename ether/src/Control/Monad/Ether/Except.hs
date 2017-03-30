{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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

import Control.Monad.Ether.Except.Class
import qualified Control.Monad.Trans.Ether.Handler as D
import qualified Control.Monad.Except as T
import Control.Monad.Signatures (Catch)
import Data.Functor.Identity
import Data.Coerce

-- | Runs an 'Except' and handles the exception with the given function.
handle :: forall tag e a . (e -> a) -> Except tag e a -> a
handle h m = runIdentity (handleT @tag h m)

-- | Runs an 'ExceptT' and handles the exception with the given function.
handleT :: forall tag e m a . Functor m => (e -> a) -> ExceptT tag e m a -> m a
handleT h m = fmap (either h id) (runExceptT @tag m)

-- | Encode type-level information for 'ExceptT'.
data EXCEPT

-- | The parameterizable exception monad.
--
-- Computations are either exceptions or normal values.
--
-- The 'return' function returns a normal value, while '>>=' exits on
-- the first exception.
type Except tag e = ExceptT tag e Identity

-- | The exception monad transformer.
--
-- The 'return' function returns a normal value, while '>>=' exits on
-- the first exception.
type ExceptT tag e = D.Handler '(EXCEPT, tag) (T.ExceptT e)

-- | Runs an 'Except' and returns either an exception or a normal value.
runExcept :: forall tag e a . Except tag e a -> Either e a
runExcept = coerce (T.runExcept @e @a)

-- | Runs an 'ExceptT' and returns either an exception or a normal value.
runExceptT :: forall tag e m a . ExceptT tag e m a -> m (Either e a)
runExceptT = coerce (T.runExceptT @e @m @a)

-- | Constructor for computations in the exception monad transformer.
exceptT :: forall tag e m a . m (Either e a) -> ExceptT tag e m a
exceptT = coerce (T.ExceptT @e @m @a)

instance
    ( T.MonadError e (trans m) -- FIXME: (forall m . T.MonadError e (trans m))
    ) => MonadExcept tag e (D.Handler '(EXCEPT, tag) trans (m :: * -> *))
  where
    throw =
      coerce (T.throwError @e @(trans m) @a) ::
        forall dp a . e -> D.Handler dp trans m a
    {-# INLINE throw #-}

    catch =
      coerce (T.catchError @e @(trans m) @a) ::
        forall dp a . Catch e (D.Handler dp trans m) a
    {-# INLINE catch #-}
