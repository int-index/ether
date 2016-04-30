{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

-- | See "Control.Monad.Ether.Reader".

module Control.Monad.Ether.Ambiguous.Reader
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
import Control.Monad.Ether.Reader (MonadReader, ReaderT, Reader)
import qualified Control.Monad.Ether.Reader as Explicit

-- | See 'Control.Monad.Ether.Reader.readerT'.
readerT :: forall tag r m a . (r -> m a) -> ReaderT tag r m a
readerT = Explicit.readerT proxy#

-- | See 'Control.Monad.Ether.Reader.runReaderT'.
runReaderT :: forall tag r m a . ReaderT tag r m a -> r -> m a
runReaderT = Explicit.runReaderT proxy#

-- | See 'Control.Monad.Ether.Reader.runReader'.
runReader :: forall tag r a . Reader tag r a -> r -> a
runReader = Explicit.runReader proxy#

-- | See 'Control.Monad.Ether.Reader.local'.
local :: forall tag r m a . MonadReader tag r m => (r -> r) -> m a -> m a
local = Explicit.local (proxy# :: Proxy# tag)

-- | See 'Control.Monad.Ether.Reader.ask'.
ask :: forall tag r m . MonadReader tag r m => m r
ask = Explicit.ask (proxy# :: Proxy# tag)

-- | See 'Control.Monad.Ether.Reader.reader'.
reader :: forall tag r m a . MonadReader tag r m => (r -> a) -> m a
reader = Explicit.reader (proxy# :: Proxy# tag)

-- | See 'Control.Monad.Ether.Reader.asks'.
asks :: forall tag r m a . MonadReader tag r m => (r -> a) -> m a
asks = Explicit.asks (proxy# :: Proxy# tag)
