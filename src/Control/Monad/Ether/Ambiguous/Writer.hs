{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

-- | See "Control.Monad.Ether.Writer".

module Control.Monad.Ether.Ambiguous.Writer
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
import Control.Monad.Ether.Writer (MonadWriter, WriterT, Writer)
import qualified Control.Monad.Ether.Writer as Explicit

-- | See 'Control.Monad.Ether.Writer.writer'.
writer :: forall tag w m a . MonadWriter tag w m => (a, w) -> m a
writer = Explicit.writer (proxy# :: Proxy# tag)

-- | See 'Control.Monad.Ether.Writer.tell'.
tell :: forall tag w m . MonadWriter tag w m => w -> m ()
tell = Explicit.tell (proxy# :: Proxy# tag)

-- | See 'Control.Monad.Ether.Writer.listen'.
listen :: forall tag w m a . MonadWriter tag w m => m a -> m (a, w)
listen = Explicit.listen (proxy# :: Proxy# tag)

-- | See 'Control.Monad.Ether.Writer.pass'.
pass :: forall tag w m a . MonadWriter tag w m => m (a, w -> w) -> m a
pass = Explicit.pass (proxy# :: Proxy# tag)

-- | See 'Control.Monad.Ether.Writer.listens'.
listens :: forall tag w m a b . MonadWriter tag w m => (w -> b) -> m a -> m (a, b)
listens = Explicit.listens (proxy# :: Proxy# tag)

-- | See 'Control.Monad.Ether.Writer.censor'.
censor :: forall tag w m a . MonadWriter tag w m => (w -> w) -> m a -> m a
censor = Explicit.censor (proxy# :: Proxy# tag)

-- | See 'Control.Monad.Ether.Writer.runWriter'.
runWriter :: forall tag w a . Writer tag w a -> (a, w)
runWriter = Explicit.runWriter proxy#

-- | See 'Control.Monad.Ether.Writer.execWriter'.
execWriter :: forall tag w a . Writer tag w a -> w
execWriter = Explicit.execWriter proxy#

-- | See 'Control.Monad.Ether.Writer.writerT'.
writerT :: forall tag w m a . m (a, w) -> WriterT tag w m a
writerT = Explicit.writerT proxy#

-- | See 'Control.Monad.Ether.Writer.runWriterT'.
runWriterT :: forall tag w m a . WriterT tag w m a -> m (a, w)
runWriterT = Explicit.runWriterT proxy#

-- | See 'Control.Monad.Ether.Writer.execWriterT'.
execWriterT :: forall tag w m a . Monad m => WriterT tag w m a -> m w
execWriterT = Explicit.execWriterT proxy#
