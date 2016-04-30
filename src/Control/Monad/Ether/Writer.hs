{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Monad.Trans.Ether.Writer hiding (writer, tell, listen, pass)

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
