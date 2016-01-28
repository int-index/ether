{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

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
writerT :: proxy tag -> m (a, w) -> WriterT tag w m a
writerT _ = pack . Trans.WriterT

-- | Constructor for computations in the writer monad
-- (the inverse of 'runWriter').
writer :: Monad m => proxy tag -> (a, w) -> WriterT tag w m a
writer _ = pack . Trans.writer

-- | Runs a 'WriterT' and returns both the normal value
-- and the final accumulator.
runWriterT :: proxy tag -> WriterT tag w m a -> m (a, w)
runWriterT _ = Trans.runWriterT . unpack

-- | Runs a 'Writer' and returns both the normal value
-- and the final accumulator.
runWriter :: proxy tag -> Writer tag w a -> (a, w)
runWriter _ = Trans.runWriter . unpack

-- | Runs a 'WriterT' and returns the final accumulator,
-- discarding the normal value.
execWriterT :: Monad m => proxy tag -> WriterT tag w m a -> m w
execWriterT _ = Trans.execWriterT . unpack

-- | Runs a 'Writer' and returns the final accumulator,
-- discarding the normal value.
execWriter :: proxy tag -> Writer tag w a -> w
execWriter _ = Trans.execWriter . unpack

-- | Appends a value to the accumulator within the monad.
tell :: Monad m => proxy tag -> w -> WriterT tag w m ()
tell t w = writer t ((), w)

-- | Executes an action and adds its accumulator to the value of the computation.
listen :: (Monoid w, Monad m) => proxy tag -> WriterT tag w m a -> WriterT tag w m (a, w)
listen _ = pack . Trans.listen . unpack

-- | Executes an action which returns a value and a function, and returns the
-- value, applying the function to the accumulator.
pass :: (Monoid w, Monad m) => proxy tag -> WriterT tag w m (a, w -> w) -> WriterT tag w m a
pass _ = pack . Trans.pass . unpack
