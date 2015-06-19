{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Data.Proxy
import qualified Control.Monad.Ether.Reader as Explicit

-- | See 'Control.Monad.Ether.Reader.ReaderT'.
type ReaderT r = Explicit.ReaderT r r

-- | See 'Control.Monad.Ether.Reader.Reader'.
type Reader  r = Explicit.Reader  r r

-- | See 'Control.Monad.Ether.Reader.readerT'.
readerT :: (r -> m a) -> ReaderT r m a
readerT = Explicit.readerT Proxy

-- | See 'Control.Monad.Ether.Reader.runReaderT'.
runReaderT :: ReaderT r m a -> r -> m a
runReaderT = Explicit.runReaderT Proxy

-- | See 'Control.Monad.Ether.Reader.runReader'.
runReader :: Reader r a -> r -> a
runReader = Explicit.runReader Proxy

-- | See 'Control.Monad.Ether.Reader.MonadReader'.
type MonadReader r = Explicit.MonadReader r r

-- | See 'Control.Monad.Ether.Reader.local'.
local :: forall m r a . MonadReader r m => (r -> r) -> m a -> m a
local = Explicit.local (Proxy :: Proxy r)

-- | See 'Control.Monad.Ether.Reader.ask'.
ask :: forall m r . MonadReader r m => m r
ask = Explicit.ask (Proxy :: Proxy r)

-- | See 'Control.Monad.Ether.Reader.reader'.
reader :: forall m r a . MonadReader r m => (r -> a) -> m a
reader = Explicit.reader (Proxy :: Proxy r)

-- | See 'Control.Monad.Ether.Reader.asks'.
asks :: forall m r a . MonadReader r m => (r -> a) -> m a
asks = Explicit.asks (Proxy :: Proxy r)
