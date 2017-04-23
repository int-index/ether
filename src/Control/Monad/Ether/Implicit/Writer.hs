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

import qualified Control.Monad.Ether.Writer as A

-- | See 'Control.Monad.Ether.Writer.MonadWriter'.
type MonadWriter w = A.MonadWriter w w

-- | See 'Control.Monad.Ether.Writer.writer'.
writer :: forall w m a . MonadWriter w m => (a, w) -> m a
writer = A.writer @w

-- | See 'Control.Monad.Ether.Writer.tell'.
tell :: forall w m . MonadWriter w m => w -> m ()
tell = A.tell @w

-- | See 'Control.Monad.Ether.Writer.listen'.
listen :: forall w m a . MonadWriter w m => m a -> m (a, w)
listen = A.listen @w

-- | See 'Control.Monad.Ether.Writer.pass'.
pass :: forall w m a . MonadWriter w m => m (a, w -> w) -> m a
pass = A.pass @w

-- | See 'Control.Monad.Ether.Writer.listens'.
listens :: forall w m a b . MonadWriter w m => (w -> b) -> m a -> m (a, b)
listens = A.listens @w

-- | See 'Control.Monad.Ether.Writer.censor'.
censor :: forall w m a . MonadWriter w m => (w -> w) -> m a -> m a
censor = A.censor @w

-- | See 'Control.Monad.Ether.Writer.Writer'.
type Writer w = A.Writer w w

-- | See 'Control.Monad.Ether.Writer.runWriter'.
runWriter :: Writer w a -> (a, w)
runWriter = A.runWriter

-- | See 'Control.Monad.Ether.Writer.execWriter'.
execWriter :: Writer w a -> w
execWriter = A.execWriter

-- | See 'Control.Monad.Ether.Writer.WriterT'.
type WriterT w = A.WriterT w w

-- | See 'Control.Monad.Ether.Writer.writerT'.
writerT :: m (a, w) -> WriterT w m a
writerT = A.writerT

-- | See 'Control.Monad.Ether.Writer.runWriterT'.
runWriterT :: WriterT w m a -> m (a, w)
runWriterT = A.runWriterT

-- | See 'Control.Monad.Ether.Writer.execWriterT'.
execWriterT :: Monad m => WriterT w m a -> m w
execWriterT = A.execWriterT
