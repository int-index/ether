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
import Control.Ether.TT

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
type WriterT tag w = TT tag (Trans.WriterT w)

tagged :: proxy tag -> Trans.WriterT w m a -> WriterT tag w m a
tagged _ = pack

untagged :: proxy tag -> WriterT tag w m a -> Trans.WriterT w m a
untagged _ = unpack

-- | Constructor for computations in the writer monad transformer.
writerT :: proxy tag -> m (a, w) -> WriterT tag w m a
writerT t = tagged t . Trans.WriterT

-- | Constructor for computations in the writer monad
-- (the inverse of 'runWriter').
writer :: Monad m => proxy tag -> (a, w) -> WriterT tag w m a
writer t = tagged t . Trans.writer

-- | Runs a 'WriterT' and returns both the normal value
-- and the final accumulator.
runWriterT :: proxy tag -> WriterT tag w m a -> m (a, w)
runWriterT t = Trans.runWriterT . untagged t

-- | Runs a 'Writer' and returns both the normal value
-- and the final accumulator.
runWriter :: proxy tag -> Writer tag w a -> (a, w)
runWriter t = Trans.runWriter . untagged t

-- | Runs a 'WriterT' and returns the final accumulator,
-- discarding the normal value.
execWriterT :: Monad m => proxy tag -> WriterT tag w m a -> m w
execWriterT t = Trans.execWriterT . untagged t

-- | Runs a 'Writer' and returns the final accumulator,
-- discarding the normal value.
execWriter :: proxy tag -> Writer tag w a -> w
execWriter t = Trans.execWriter . untagged t

-- | Appends a value to the accumulator within the monad.
tell :: Monad m => proxy tag -> w -> WriterT tag w m ()
tell t w = writer t ((), w)

-- | Executes an action and adds its accumulator to the value of the computation.
listen :: (Monoid w, Monad m) => proxy tag -> WriterT tag w m a -> WriterT tag w m (a, w)
listen t m = tagged t $ Trans.listen (untagged t m)

-- | Executes an action which returns a value and a function, and returns the
-- value, applying the function to the accumulator.
pass :: (Monoid w, Monad m) => proxy tag -> WriterT tag w m (a, w -> w) -> WriterT tag w m a
pass t m = tagged t $ Trans.pass (untagged t m)
