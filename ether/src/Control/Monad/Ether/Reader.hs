{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
import qualified Control.Monad.Trans.Ether.Dispatch as D
import qualified Control.Monad.Trans.Reader as T
import Data.Functor.Identity

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

-- | Encode type-level information for 'ReaderT'.
data K_READER t r = READER t r

-- | The parameterizable reader monad.
--
-- Computations are functions of a shared environment.
--
-- The 'return' function ignores the environment, while '>>=' passes
-- the inherited environment to both subcomputations.
type Reader tag r = ReaderT tag r Identity

-- | The reader monad transformer,
-- which adds a read-only environment to the given monad.
--
-- The 'return' function ignores the environment, while '>>=' passes
-- the inherited environment to both subcomputations.
type ReaderT tag r = D.Dispatch (READER tag r) (T.ReaderT r)

-- | Constructor for computations in the reader monad transformer.
readerT :: forall tag r m a . (r -> m a) -> ReaderT tag r m a
readerT = D.pack . T.ReaderT

-- | Runs a 'ReaderT' with the given environment
-- and returns the final value.
runReaderT :: forall tag r m a . ReaderT tag r m a -> r -> m a
runReaderT = T.runReaderT . D.unpack

-- | Runs a 'ReaderT' with the given environment
-- and returns the final value.
runReader :: forall tag r a . Reader tag r a -> r -> a
runReader = T.runReader . D.unpack

instance
    ( Monad m, r ~ r', trans ~ T.ReaderT r
    ) => MonadReader tag r (D.Dispatch (READER tag r') trans m)
  where
    ask _ = D.pack T.ask
    local _ f = D.pack . T.withReaderT f . D.unpack
    reader _ = D.pack . T.reader
