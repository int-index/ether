{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | See "Control.Monad.Trans.Writer".

module Control.Monad.Trans.Ether.Writer
    (
    -- * The Writer monad
      Writer
    , writer
    , runWriter
    , execWriter
    -- * The WriterT monad transformer
    , WriterT
    , writerT
    , runWriterT
    , execWriterT
    -- * Writer operations
    , tell
    , listen
    , pass
    ) where

import Data.Functor.Identity (Identity(..))
import qualified Control.Monad.Trans.Writer.Lazy as Trans
import Control.Monad.Trans.Ether.Tagged

-- | The parametrizable writer monad.
--
-- Computations can accumulate a monoid value.
--
-- The 'return' function produces the output 'mempty', while '>>=' combines
-- the outputs of the subcomputations using 'mappend'.
type Writer tag w = WriterT tag w Identity

-- | The writer monad transformer.
--
-- The 'return' function produces the output 'mempty', while '>>=' combines
-- the outputs of the subcomputations using 'mappend'.
type WriterT tag w = TaggedTrans tag (Trans.WriterT w)

-- | Constructor for computations in the writer monad transformer.
writerT :: forall tag w m a . m (a, w) -> WriterT tag w m a
writerT = pack . Trans.WriterT

-- | Constructor for computations in the writer monad
-- (the inverse of 'runWriter').
writer :: forall tag w m a . Monad m => (a, w) -> WriterT tag w m a
writer = pack . Trans.writer

-- | Runs a 'WriterT' and returns both the normal value
-- and the final accumulator.
runWriterT :: forall tag w m a . WriterT tag w m a -> m (a, w)
runWriterT = Trans.runWriterT . unpack

-- | Runs a 'Writer' and returns both the normal value
-- and the final accumulator.
runWriter :: forall tag w a . Writer tag w a -> (a, w)
runWriter = Trans.runWriter . unpack

-- | Runs a 'WriterT' and returns the final accumulator,
-- discarding the normal value.
execWriterT :: forall tag w m a . Monad m => WriterT tag w m a -> m w
execWriterT = Trans.execWriterT . unpack

-- | Runs a 'Writer' and returns the final accumulator,
-- discarding the normal value.
execWriter :: forall tag w a . Writer tag w a -> w
execWriter = Trans.execWriter . unpack

-- | Appends a value to the accumulator within the monad.
tell :: forall tag w m . Monad m => w -> WriterT tag w m ()
tell w = writer @tag ((), w)

-- | Executes an action and adds its accumulator to the value of the computation.
listen :: forall tag w m a . Monad m => WriterT tag w m a -> WriterT tag w m (a, w)
listen = pack . Trans.listen . unpack

-- | Executes an action which returns a value and a function, and returns the
-- value, applying the function to the accumulator.
pass :: forall tag w m a . Monad m => WriterT tag w m (a, w -> w) -> WriterT tag w m a
pass = pack . Trans.pass . unpack
