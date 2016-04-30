{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

-- | See "Control.Monad.Trans.Except".

module Control.Monad.Trans.Ether.Except
    (
    -- * The Except monad
      Except
    , except
    , runExcept
    -- * The ExceptT monad transformer
    , ExceptT
    , exceptT
    , runExceptT
    -- * Exception operations
    , throw
    , catch
    ) where

import Data.Functor.Identity (Identity(..))
import qualified Control.Monad.Trans.Except as Trans
import Control.Monad.Trans.Ether.Tagged


-- | The parameterizable exception monad.
--
-- Computations are either exceptions or normal values.
--
-- The 'return' function returns a normal value, while '>>=' exits on
-- the first exception.
type Except tag e = ExceptT tag e Identity

-- | Runs an 'Except' and returns either an exception or a normal value.
runExcept :: forall tag e a . Except tag e a -> Either e a
runExcept = Trans.runExcept . unpack

-- | The exception monad transformer.
--
-- The 'return' function returns a normal value, while '>>=' exits on
-- the first exception.
type ExceptT tag e = TaggedTrans tag (Trans.ExceptT e)

-- | Constructor for computations in the exception monad transformer.
exceptT :: forall tag e m a . m (Either e a) -> ExceptT tag e m a
exceptT = pack . Trans.ExceptT

-- | Constructor for computations in the exception monad
-- (the inverse of 'runExcept').
except :: forall tag e m a . Monad m => Either e a -> ExceptT tag e m a
except = exceptT @tag . return

-- | Runs an 'ExceptT' and returns either an exception or a normal value.
runExceptT :: forall tag e m a . ExceptT tag e m a -> m (Either e a)
runExceptT = Trans.runExceptT . unpack

-- | Is used within a monadic computation to begin exception processing.
throw :: forall tag e m a . Monad m => e -> ExceptT tag e m a
throw = pack . Trans.throwE

-- | A handler function to handle previous exceptions and return to normal execution.
catch
  :: forall tag e m a . Monad m
  => ExceptT tag e m a -> (e -> ExceptT tag e m a) -> ExceptT tag e m a
catch m h = pack $ Trans.catchE (unpack m) (unpack . h)
