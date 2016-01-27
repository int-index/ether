{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | See "Control.Monad.Ether.Writer".

module Control.Monad.Ether.Implicit.Writer
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

import Data.Proxy
import qualified Control.Monad.Ether.Writer as Explicit

-- | See 'Control.Monad.Ether.Writer.MonadWriter'.
type MonadWriter w = Explicit.MonadWriter w w

-- | See 'Control.Monad.Ether.Writer.writer'.
writer :: forall w m a . MonadWriter w m => (a, w) -> m a
writer = Explicit.writer (Proxy :: Proxy w)

-- | See 'Control.Monad.Ether.Writer.tell'.
tell :: forall w m . MonadWriter w m => w -> m ()
tell = Explicit.tell (Proxy :: Proxy w)

-- | See 'Control.Monad.Ether.Writer.listen'.
listen :: forall w m a . MonadWriter w m => m a -> m (a, w)
listen = Explicit.listen (Proxy :: Proxy w)

-- | See 'Control.Monad.Ether.Writer.pass'.
pass :: forall w m a . MonadWriter w m => m (a, w -> w) -> m a
pass = Explicit.pass (Proxy :: Proxy w)

-- | See 'Control.Monad.Ether.Writer.listens'.
listens :: forall w m a b . MonadWriter w m => (w -> b) -> m a -> m (a, b)
listens = Explicit.listens (Proxy :: Proxy w)

-- | See 'Control.Monad.Ether.Writer.censor'.
censor :: forall w m a . MonadWriter w m => (w -> w) -> m a -> m a
censor = Explicit.censor (Proxy :: Proxy w)

-- | See 'Control.Monad.Ether.Writer.Writer'.
type Writer w = Explicit.Writer w w

-- | See 'Control.Monad.Ether.Writer.runWriter'.
runWriter :: Writer w a -> (a, w)
runWriter = Explicit.runWriter Proxy

-- | See 'Control.Monad.Ether.Writer.execWriter'.
execWriter :: Writer w a -> w
execWriter = Explicit.execWriter Proxy

-- | See 'Control.Monad.Ether.Writer.WriterT'.
type WriterT w = Explicit.WriterT w w

-- | See 'Control.Monad.Ether.Writer.writerT'.
writerT :: m (a, w) -> WriterT w m a
writerT = Explicit.writerT Proxy

-- | See 'Control.Monad.Ether.Writer.runWriterT'.
runWriterT :: WriterT w m a -> m (a, w)
runWriterT = Explicit.runWriterT Proxy

-- | See 'Control.Monad.Ether.Writer.execWriterT'.
execWriterT :: Monad m => WriterT w m a -> m w
execWriterT = Explicit.execWriterT Proxy
