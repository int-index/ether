{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}

-- | See "Control.Monad.Writer".

module Control.Monad.Ether.Writer
    (
    -- * MonadWriter class
      MonadWriter
    , writer
    , tell
    , listen
    , pass
    , listens
    , censor
    -- * The Writer monad
    , Writer
    , runWriter
    , execWriter
    -- * The WriterT monad transformer
    , WriterT
    , writerT
    , runWriterT
    , execWriterT
    ) where

import GHC.Prim (Proxy#, proxy#)
import Control.Monad.Ether.Writer.Class (MonadWriter)
import qualified Control.Monad.Ether.Writer.Class as C
import qualified Control.Monad.Trans.Ether.Dispatch as D
import qualified Control.Monad.Trans.Writer as T
import Data.Functor.Identity

-- | Embed a simple writer action.
writer :: forall tag w m a . MonadWriter tag w m => (a, w) -> m a
writer = C.writer (proxy# :: Proxy# tag)

-- | Append a value to the accumulator within the monad.
tell :: forall tag w m . MonadWriter tag w m => w -> m ()
tell = C.tell (proxy# :: Proxy# tag)

-- | Execute an action and add its accumulator
-- to the value of the computation.
listen :: forall tag w m a . MonadWriter tag w m => m a -> m (a, w)
listen = C.listen (proxy# :: Proxy# tag)

-- | Execute an action which returns a value and a function,
-- and return the value, applying the function to the accumulator.
pass :: forall tag w m a . MonadWriter tag w m => m (a, w -> w) -> m a
pass = C.pass (proxy# :: Proxy# tag)

-- | Execute an action and add the result of applying the given function to
-- its accumulator to the value of the computation.
listens :: forall tag w m a b . MonadWriter tag w m => (w -> b) -> m a -> m (a, b)
listens f m = do
  ~(a, w) <- listen @tag m
  return (a, f w)

-- | Execute an action and apply a function to its accumulator.
censor :: forall tag w m a . MonadWriter tag w m => (w -> w) -> m a -> m a
censor f m = pass @tag $ do
  a <- m
  return (a, f)

-- | Encode type-level information for 'WriterT'.
data K_WRITER t w = WRITER t w

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
type WriterT tag w = D.Dispatch (WRITER tag w) (T.WriterT w)

-- | Constructor for computations in the writer monad transformer.
writerT :: forall tag w m a . m (a, w) -> WriterT tag w m a
writerT = D.pack . T.WriterT

-- | Runs a 'WriterT' and returns both the normal value
-- and the final accumulator.
runWriterT :: forall tag w m a . WriterT tag w m a -> m (a, w)
runWriterT = T.runWriterT . D.unpack

-- | Runs a 'Writer' and returns both the normal value
-- and the final accumulator.
runWriter :: forall tag w a . Writer tag w a -> (a, w)
runWriter = T.runWriter . D.unpack

-- | Runs a 'WriterT' and returns the final accumulator,
-- discarding the normal value.
execWriterT :: forall tag w m a . Monad m => WriterT tag w m a -> m w
execWriterT = T.execWriterT . D.unpack

-- | Runs a 'Writer' and returns the final accumulator,
-- discarding the normal value.
execWriter :: forall tag w a . Writer tag w a -> w
execWriter = T.execWriter . D.unpack

instance
    ( Monoid w, Monad m, w ~ w', trans ~ T.WriterT w
    ) => MonadWriter tag w (D.Dispatch (WRITER tag w') trans m)
  where
    writer _ = D.pack . T.writer
    tell _ = D.pack . T.tell
    listen _ = D.pack . T.listen . D.unpack
    pass _ = D.pack . T.pass . D.unpack
