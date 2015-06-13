{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    , mapReaderT
    ) where

import Data.Proxy
import qualified Control.Monad.Ether.Reader as Explicit

type ReaderT r = Explicit.ReaderT r r
type Reader  r = Explicit.Reader  r r

readerT :: (r -> m a) -> ReaderT r m a
readerT = Explicit.readerT Proxy

runReaderT :: ReaderT r m a -> r -> m a
runReaderT = Explicit.runReaderT Proxy

runReader :: Reader r a -> r -> a
runReader = Explicit.runReader Proxy

mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT = Explicit.mapReaderT Proxy

type MonadReader r = Explicit.MonadReader r r

local :: forall m r a . MonadReader r m => (r -> r) -> m a -> m a
local = Explicit.local (Proxy :: Proxy r)

ask :: forall m r . MonadReader r m => m r
ask = Explicit.ask (Proxy :: Proxy r)

reader :: forall m r a . MonadReader r m => (r -> a) -> m a
reader = Explicit.reader (Proxy :: Proxy r)

asks :: forall m r a . MonadReader r m => (r -> a) -> m a
asks = Explicit.asks (Proxy :: Proxy r)

