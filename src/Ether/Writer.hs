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
  -- * Internal labels
  , TAGGED
  , WRITER
  ) where

import Control.Monad.Signatures (Listen, Pass)
import qualified Control.Monad.Trans.Lift.Listen as Lift
import qualified Control.Monad.Trans.Lift.Pass   as Lift
import qualified Control.Monad.Writer as T
import Data.Coerce
import Data.Functor.Identity

import Ether.Handler
import Ether.Internal

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
    {-# INLINE writer #-}

    tell   = Lift.lift . tell @tag
    {-# INLINE tell #-}

    listen = Lift.liftListen (listen @tag)
    {-# INLINE listen #-}

    pass   = Lift.liftPass (pass @tag)
    {-# INLINE pass #-}

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
        forall eff a . (a, w) -> Handler eff trans m a
    {-# INLINE writer #-}

    tell =
      handling @WRITER @w @trans @m $
      coerce (T.tell @w @(trans m))
    {-# INLINE tell #-}

    listen =
      handling @WRITER @w @trans @m $
      coerce (T.listen @w @(trans m) @a) ::
        forall eff a . Listen w (Handler eff trans m) a
    {-# INLINE listen #-}

    pass =
      handling @WRITER @w @trans @m $
      coerce (T.pass @w @(trans m) @a) ::
        forall eff a . Pass w (Handler eff trans m) a
    {-# INLINE pass #-}

type Writer' w = Writer w w

runWriter' :: Writer' w a -> (a, w)
runWriter' = runWriter

execWriter' :: Writer' w a -> w
execWriter' = execWriter

type WriterT' w = WriterT w w

writerT' :: m (a, w) -> WriterT' w m a
writerT' = writerT

runWriterT' :: WriterT' w m a -> m (a, w)
runWriterT' = runWriterT

execWriterT' :: Monad m => WriterT' w m a -> m w
execWriterT' = execWriterT

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
