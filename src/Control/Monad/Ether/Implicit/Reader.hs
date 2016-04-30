{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

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

import GHC.Prim (Proxy#, proxy#)
import qualified Control.Monad.Ether.Reader as Explicit

-- | See 'Control.Monad.Ether.Reader.ReaderT'.
type ReaderT r = Explicit.ReaderT r r

-- | See 'Control.Monad.Ether.Reader.Reader'.
type Reader  r = Explicit.Reader  r r

-- | See 'Control.Monad.Ether.Reader.readerT'.
readerT :: (r -> m a) -> ReaderT r m a
readerT = Explicit.readerT proxy#

-- | See 'Control.Monad.Ether.Reader.runReaderT'.
runReaderT :: ReaderT r m a -> r -> m a
runReaderT = Explicit.runReaderT proxy#

-- | See 'Control.Monad.Ether.Reader.runReader'.
runReader :: Reader r a -> r -> a
runReader = Explicit.runReader proxy#

-- | See 'Control.Monad.Ether.Reader.MonadReader'.
type MonadReader r = Explicit.MonadReader r r

-- | See 'Control.Monad.Ether.Reader.local'.
local :: forall r m a . MonadReader r m => (r -> r) -> m a -> m a
local = Explicit.local (proxy# :: Proxy# r)

-- | See 'Control.Monad.Ether.Reader.ask'.
ask :: forall r m . MonadReader r m => m r
ask = Explicit.ask (proxy# :: Proxy# r)

-- | See 'Control.Monad.Ether.Reader.reader'.
reader :: forall r m a . MonadReader r m => (r -> a) -> m a
reader = Explicit.reader (proxy# :: Proxy# r)

-- | See 'Control.Monad.Ether.Reader.asks'.
asks :: forall r m a . MonadReader r m => (r -> a) -> m a
asks = Explicit.asks (proxy# :: Proxy# r)
