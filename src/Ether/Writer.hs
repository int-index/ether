module Ether.Writer
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
  -- * The Writer monad (lazy)
  , LazyWriter
  , runLazyWriter
  , execLazyWriter
  -- * The WriterT monad transformer (lazy)
  , LazyWriterT
  , lazyWriterT
  , runLazyWriterT
  , execLazyWriterT
  -- * MonadWriter class (implicit)
  , MonadWriter'
  , writer'
  , tell'
  , listen'
  , pass'
  , listens'
  , censor'
  -- * The Writer monad (implicit)
  , Writer'
  , runWriter'
  , execWriter'
  -- * The WriterT monad transformer (implicit)
  , WriterT'
  , writerT'
  , runWriterT'
  , execWriterT'
  -- * The Writer monad (lazy, implicit)
  , LazyWriter'
  , runLazyWriter'
  , execLazyWriter'
  -- * The WriterT monad transformer (lazy, implicit)
  , LazyWriterT'
  , lazyWriterT'
  , runLazyWriterT'
  , execLazyWriterT'
  -- * Internal labels
  , TAGGED
  , WRITER
  ) where

import Control.Monad.Signatures (Listen, Pass)
import qualified Control.Monad.Trans.Lift.Listen as Lift
import qualified Control.Monad.Trans.Lift.Pass   as Lift
import qualified Control.Monad.Writer.Class as T
import qualified Control.Monad.Writer.CPS as T.CPS
import qualified Control.Monad.Writer.Lazy as T.Lazy
import Data.Coerce
import Data.Functor.Identity

import Ether.Internal
import Ether.TaggedTrans

class (Monoid w, Monad m) => MonadWriter tag w m | m tag -> w where

    {-# MINIMAL (writer | tell), listen, pass #-}

    -- | Embed a simple writer action.
    writer :: (a, w) -> m a
    writer ~(a, w) = a <$ tell @tag w

    -- | Append a value to the accumulator within the monad.
    tell :: w -> m ()
    tell w = writer @tag ((),w)

    -- | Execute an action and add its accumulator
    -- to the value of the computation.
    listen :: m a -> m (a, w)

    -- | Execute an action which returns a value and a function,
    -- and return the value, applying the function to the accumulator.
    pass :: m (a, w -> w) -> m a

instance {-# OVERLAPPABLE #-}
         ( Lift.LiftListen t
         , Lift.LiftPass   t
         , Monad (t m)
         , MonadWriter tag w m
         , Monoid w
         ) => MonadWriter tag w (t m) where
    writer = Lift.lift . writer @tag
    tell   = Lift.lift . tell @tag
    listen = Lift.liftListen (listen @tag)
    pass   = Lift.liftPass (pass @tag)

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

type instance HandleSuper      WRITER w trans   = Monoid w
type instance HandleConstraint WRITER w trans m =
  T.MonadWriter w (trans m)

instance Monoid w => Handle WRITER w (T.CPS.WriterT w) where
  handling r = r

instance Monoid w => Handle WRITER w (T.Lazy.WriterT w) where
  handling r = r

instance
    ( Handle WRITER w trans
    , Monad m, Monad (trans m)
    ) => MonadWriter tag w (TaggedTrans (TAGGED WRITER tag) trans m)
  where

    writer =
      handling @WRITER @w @trans @m $
      coerce (T.writer @w @(trans m) @a) ::
        forall eff a . (a, w) -> TaggedTrans eff trans m a

    tell =
      handling @WRITER @w @trans @m $
      coerce (T.tell @w @(trans m))

    listen =
      handling @WRITER @w @trans @m $
      coerce (T.listen @w @(trans m) @a) ::
        forall eff a . Listen w (TaggedTrans eff trans m) a

    pass =
      handling @WRITER @w @trans @m $
      coerce (T.pass @w @(trans m) @a) ::
        forall eff a . Pass w (TaggedTrans eff trans m) a

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
type WriterT tag w = TaggedTrans (TAGGED WRITER tag) (T.CPS.WriterT w)

-- | Constructor for computations in the writer monad transformer.
writerT :: forall tag w m a . (Functor m, Monoid w) => m (a, w) -> WriterT tag w m a
writerT = coerce (T.CPS.writerT @m @w @a)

-- | Runs a 'WriterT' and returns both the normal value
-- and the final accumulator.
runWriterT :: forall tag w m a . Monoid w => WriterT tag w m a -> m (a, w)
runWriterT = coerce (T.CPS.runWriterT @w @m @a)

-- | Runs a 'Writer' and returns both the normal value
-- and the final accumulator.
runWriter :: forall tag w a . Monoid w => Writer tag w a -> (a, w)
runWriter = coerce (T.CPS.runWriter @w @a)

-- | Runs a 'WriterT' and returns the final accumulator,
-- discarding the normal value.
execWriterT :: forall tag w m a . (Monad m, Monoid w) => WriterT tag w m a -> m w
execWriterT = coerce (T.CPS.execWriterT @m @w @a)

-- | Runs a 'Writer' and returns the final accumulator,
-- discarding the normal value.
execWriter :: forall tag w a . Monoid w => Writer tag w a -> w
execWriter = coerce (T.CPS.execWriter @w @a)

-- | The parametrizable writer monad.
--
-- Computations can accumulate a monoid value.
--
-- The 'return' function produces the output 'mempty', while '>>=' combines
-- the outputs of the subcomputations using 'mappend'.
type LazyWriter tag w = LazyWriterT tag w Identity

-- | The writer monad transformer.
--
-- The 'return' function produces the output 'mempty', while '>>=' combines
-- the outputs of the subcomputations using 'mappend'.
type LazyWriterT tag w = TaggedTrans (TAGGED WRITER tag) (T.Lazy.WriterT w)

-- | Constructor for computations in the writer monad transformer.
lazyWriterT :: forall tag w m a . m (a, w) -> LazyWriterT tag w m a
lazyWriterT = coerce (T.Lazy.WriterT @w @m @a)

-- | Runs a 'WriterT' and returns both the normal value
-- and the final accumulator.
runLazyWriterT :: forall tag w m a . LazyWriterT tag w m a -> m (a, w)
runLazyWriterT = coerce (T.Lazy.runWriterT @w @m @a)

-- | Runs a 'Writer' and returns both the normal value
-- and the final accumulator.
runLazyWriter :: forall tag w a . LazyWriter tag w a -> (a, w)
runLazyWriter = coerce (T.Lazy.runWriter @w @a)

-- | Runs a 'WriterT' and returns the final accumulator,
-- discarding the normal value.
execLazyWriterT :: forall tag w m a . Monad m => LazyWriterT tag w m a -> m w
execLazyWriterT = coerce (T.Lazy.execWriterT @m @w @a)

-- | Runs a 'Writer' and returns the final accumulator,
-- discarding the normal value.
execLazyWriter :: forall tag w a . LazyWriter tag w a -> w
execLazyWriter = coerce (T.Lazy.execWriter @w @a)

type Writer' w = Writer w w

runWriter' :: Monoid w => Writer' w a -> (a, w)
runWriter' = runWriter

execWriter' :: Monoid w => Writer' w a -> w
execWriter' = execWriter

type WriterT' w = WriterT w w

writerT' :: (Functor m, Monoid w) => m (a, w) -> WriterT' w m a
writerT' = writerT

runWriterT' :: Monoid w => WriterT' w m a -> m (a, w)
runWriterT' = runWriterT

execWriterT' :: (Monad m, Monoid w) => WriterT' w m a -> m w
execWriterT' = execWriterT

type LazyWriter' w = LazyWriter w w

runLazyWriter' :: LazyWriter' w a -> (a, w)
runLazyWriter' = runLazyWriter

execLazyWriter' :: LazyWriter' w a -> w
execLazyWriter' = execLazyWriter

type LazyWriterT' w = LazyWriterT w w

lazyWriterT' :: m (a, w) -> LazyWriterT' w m a
lazyWriterT' = lazyWriterT

runLazyWriterT' :: LazyWriterT' w m a -> m (a, w)
runLazyWriterT' = runLazyWriterT

execLazyWriterT' :: Monad m => LazyWriterT' w m a -> m w
execLazyWriterT' = execLazyWriterT

type MonadWriter' w = MonadWriter w w

writer' :: forall w m a . MonadWriter' w m => (a, w) -> m a
writer' = writer @w

tell' :: forall w m . MonadWriter' w m => w -> m ()
tell' = tell @w

listen' :: forall w m a . MonadWriter' w m => m a -> m (a, w)
listen' = listen @w

pass' :: forall w m a . MonadWriter' w m => m (a, w -> w) -> m a
pass' = pass @w

listens' :: forall w m a b . MonadWriter' w m => (w -> b) -> m a -> m (a, b)
listens' = listens @w

censor' :: forall w m a . MonadWriter' w m => (w -> w) -> m a -> m a
censor' = censor @w
