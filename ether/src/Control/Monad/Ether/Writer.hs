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

import Control.Monad.Ether.Handle
import Control.Monad.Ether.Writer.Class
import Control.Monad.Trans.Ether.Handler
import qualified Control.Monad.Writer as T
import Data.Functor.Identity
import Control.Monad.Signatures (Listen, Pass)
import Data.Coerce

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
data WRITER

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
type WriterT tag w = Handler (TAGGED WRITER tag) (T.WriterT w)

-- | Constructor for computations in the writer monad transformer.
writerT :: forall tag w m a . m (a, w) -> WriterT tag w m a
writerT = coerce (T.WriterT @w @m @a)

-- | Runs a 'WriterT' and returns both the normal value
-- and the final accumulator.
runWriterT :: forall tag w m a . WriterT tag w m a -> m (a, w)
runWriterT = coerce (T.runWriterT @w @m @a)

-- | Runs a 'Writer' and returns both the normal value
-- and the final accumulator.
runWriter :: forall tag w a . Writer tag w a -> (a, w)
runWriter = coerce (T.runWriter @w @a)

-- | Runs a 'WriterT' and returns the final accumulator,
-- discarding the normal value.
execWriterT :: forall tag w m a . Monad m => WriterT tag w m a -> m w
execWriterT = coerce (T.execWriterT @m @w @a)

-- | Runs a 'Writer' and returns the final accumulator,
-- discarding the normal value.
execWriter :: forall tag w a . Writer tag w a -> w
execWriter = coerce (T.execWriter @w @a)

type instance HandleSuper      WRITER w trans   = Monoid w
type instance HandleConstraint WRITER w trans m =
  T.MonadWriter w (trans m)

instance Monoid w => Handle WRITER w (T.WriterT w) where
  handling r = r
  {-# INLINE handling #-}

instance
    ( Handle WRITER w trans
    , Monad m, Monad (trans m)
    ) => MonadWriter tag w (Handler (TAGGED WRITER tag) trans m)
  where

    writer =
      handling @WRITER @w @trans @m $
      coerce (T.writer @w @(trans m) @a) ::
        forall dp a . (a, w) -> Handler dp trans m a
    {-# INLINE writer #-}

    tell =
      handling @WRITER @w @trans @m $
      coerce (T.tell @w @(trans m))
    {-# INLINE tell #-}

    listen =
      handling @WRITER @w @trans @m $
      coerce (T.listen @w @(trans m) @a) ::
        forall dp a . Listen w (Handler dp trans m) a
    {-# INLINE listen #-}

    pass =
      handling @WRITER @w @trans @m $
      coerce (T.pass @w @(trans m) @a) ::
        forall dp a . Pass w (Handler dp trans m) a
    {-# INLINE pass #-}
