{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

type MonadWriter w = Explicit.MonadWriter w w

writer :: forall m w a . MonadWriter w m => (a, w) -> m a
writer = Explicit.writer (Proxy :: Proxy w)

tell :: forall m w . MonadWriter w m => w -> m ()
tell = Explicit.tell (Proxy :: Proxy w)

listen :: forall m w a . MonadWriter w m => m a -> m (a, w)
listen = Explicit.listen (Proxy :: Proxy w)

pass :: forall m w a . MonadWriter w m => m (a, w -> w) -> m a
pass = Explicit.pass (Proxy :: Proxy w)

listens :: forall m w a b . MonadWriter w m => (w -> b) -> m a -> m (a, b)
listens = Explicit.listens (Proxy :: Proxy w)

censor :: forall m w a . MonadWriter w m => (w -> w) -> m a -> m a
censor = Explicit.censor (Proxy :: Proxy w)

type Writer w = Explicit.Writer w w

runWriter :: Writer w a -> (a, w)
runWriter = Explicit.runWriter Proxy

execWriter :: Writer w a -> w
execWriter = Explicit.execWriter Proxy

type WriterT w = Explicit.WriterT w w

writerT :: m (a, w) -> WriterT w m a
writerT = Explicit.writerT Proxy

runWriterT :: WriterT w m a -> m (a, w)
runWriterT = Explicit.runWriterT Proxy

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT = Explicit.execWriterT Proxy
