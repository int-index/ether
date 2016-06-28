{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
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
import qualified Control.Monad.Trans.Ether.Dispatch as D
import qualified Control.Monad.Trans.Except as T
import Data.Functor.Identity

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

-- | Encode type-level information for 'ExceptT'.
data EXCEPT t e

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
type ExceptT tag e = D.Dispatch (EXCEPT tag e) (T.ExceptT e)

-- | Runs an 'Except' and returns either an exception or a normal value.
runExcept :: forall tag e a . Except tag e a -> Either e a
runExcept = T.runExcept . D.unpack

-- | Runs an 'ExceptT' and returns either an exception or a normal value.
runExceptT :: forall tag e m a . ExceptT tag e m a -> m (Either e a)
runExceptT = T.runExceptT . D.unpack

-- | Constructor for computations in the exception monad transformer.
exceptT :: forall tag e m a . m (Either e a) -> ExceptT tag e m a
exceptT = D.pack . T.ExceptT

instance
    ( Monad m, e ~ e', trans ~ T.ExceptT e
    ) => MonadExcept tag e (D.Dispatch (EXCEPT tag e') trans m)
  where
    throw _ = D.pack . T.throwE
    catch _ m h = D.pack $ T.catchE (D.unpack m) (D.unpack . h)
