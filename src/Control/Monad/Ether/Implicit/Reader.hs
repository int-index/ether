{-# LANGUAGE ConstraintKinds #-}

-- | See "Control.Monad.Ether.Reader".

module Control.Monad.Ether.Implicit.Reader
    (
    -- * MonadReader class
      MonadReader
    , local
    , ask
    , reader
    , asks
    -- * The Reader monad
    , Reader
    , runReader
    -- * The ReaderT monad transformer
    , ReaderT
    , readerT
    , runReaderT
    ) where

import qualified Control.Monad.Ether.Reader as A

-- | See 'Control.Monad.Ether.Reader.ReaderT'.
type ReaderT r = A.ReaderT r r

-- | See 'Control.Monad.Ether.Reader.readerT'.
readerT :: (r -> m a) -> ReaderT r m a
readerT = A.readerT

-- | See 'Control.Monad.Ether.Reader.runReaderT'.
runReaderT :: ReaderT r m a -> r -> m a
runReaderT = A.runReaderT

-- | See 'Control.Monad.Ether.Reader.Reader'.
type Reader r = A.Reader r r

-- | See 'Control.Monad.Ether.Reader.runReader'.
runReader :: Reader r a -> r -> a
runReader = A.runReader

-- | See 'Control.Monad.Ether.Reader.MonadReader'.
type MonadReader r = A.MonadReader r r

-- | See 'Control.Monad.Ether.Reader.local'.
local :: forall r m a . MonadReader r m => (r -> r) -> m a -> m a
local = A.local @r

-- | See 'Control.Monad.Ether.Reader.ask'.
ask :: forall r m . MonadReader r m => m r
ask = A.ask @r

-- | See 'Control.Monad.Ether.Reader.reader'.
reader :: forall r m a . MonadReader r m => (r -> a) -> m a
reader = A.reader @r

-- | See 'Control.Monad.Ether.Reader.asks'.
asks :: forall r m a . MonadReader r m => (r -> a) -> m a
asks = A.asks @r
