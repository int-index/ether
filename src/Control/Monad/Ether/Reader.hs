{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

-- | See "Control.Monad.Reader".

module Control.Monad.Ether.Reader
    (
    -- * MonadReader class
      MonadReader
    , ask
    , local
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
import Control.Monad.Ether.Reader.Class (MonadReader)
import qualified Control.Monad.Ether.Reader.Class as C
import Control.Monad.Trans.Ether.Reader hiding (reader, ask, local)

-- | Retrieves the monad environment.
ask :: forall tag r m . MonadReader tag r m => m r
ask = C.ask (proxy# :: Proxy# tag)

-- | Executes a computation in a modified environment.
local
  :: forall tag r m a
   . MonadReader tag r m
  => (r -> r)
  -- ^ The function to modify the environment.
  -> m a
  -- ^ @Reader@ to run in the modified environment.
  -> m a
local = C.local (proxy# :: Proxy# tag)

-- | Retrieves a function of the current environment.
reader
  :: forall tag r m a
   . MonadReader tag r m
  => (r -> a)
  -- ^ The selector function to apply to the environment.
  -> m a
reader = C.reader (proxy# :: Proxy# tag)

-- | Retrieves a function of the current environment.
asks
  :: forall tag r m a
   . MonadReader tag r m
  => (r -> a)
  -- ^ The selector function to apply to the environment.
  -> m a
asks = C.reader (proxy# :: Proxy# tag)
