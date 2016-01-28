{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
runExcept :: proxy tag -> Except tag e a -> Either e a
runExcept _ = Trans.runExcept . unpack

-- | The exception monad transformer.
--
-- The 'return' function returns a normal value, while '>>=' exits on
-- the first exception.
type ExceptT tag e = TaggedTrans tag (Trans.ExceptT e)

-- | Constructor for computations in the exception monad transformer.
exceptT :: proxy tag -> m (Either e a) -> ExceptT tag e m a
exceptT _ = pack . Trans.ExceptT

-- | Constructor for computations in the exception monad
-- (the inverse of 'runExcept').
except :: Monad m => proxy tag -> Either e a -> ExceptT tag e m a
except t = exceptT t . return

-- | Runs an 'ExceptT' and returns either an exception or a normal value.
runExceptT :: proxy tag -> ExceptT tag e m a -> m (Either e a)
runExceptT _ = Trans.runExceptT . unpack

-- | Is used within a monadic computation to begin exception processing.
throw :: Monad m => proxy tag -> e -> ExceptT tag e m a
throw _ = pack . Trans.throwE

-- | A handler function to handle previous exceptions and return to normal execution.
catch :: Monad m => proxy tag -> ExceptT tag e m a -> (e -> ExceptT tag e m a) -> ExceptT tag e m a
catch _ m h = pack $ Trans.catchE (unpack m) (unpack . h)
