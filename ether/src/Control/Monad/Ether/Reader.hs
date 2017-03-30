{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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

import Control.Monad.Ether.Reader.Class
import qualified Control.Monad.Trans.Ether.Handler as D
import Control.Monad.Trans.Lift.Local (Local)
import qualified Control.Monad.Reader as T
import Data.Functor.Identity
import Data.Coerce

-- | Retrieves a function of the current environment.
asks
  :: forall tag r m a
   . MonadReader tag r m
  => (r -> a)
  -- ^ The selector function to apply to the environment.
  -> m a
asks = reader @tag
{-# INLINE asks #-}

-- | Encode type-level information for 'ReaderT'.
data READER

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
type ReaderT tag r = D.Handler '(READER, tag) (T.ReaderT r)

-- | Constructor for computations in the reader monad transformer.
readerT :: forall tag r m a . (r -> m a) -> ReaderT tag r m a
readerT = coerce (T.ReaderT @r @m @a)
{-# INLINE readerT #-}

-- | Runs a 'ReaderT' with the given environment
-- and returns the final value.
runReaderT :: forall tag r m a . ReaderT tag r m a -> r -> m a
runReaderT = coerce (T.runReaderT @r @_ @m @a)
{-# INLINE runReaderT #-}

-- | Runs a 'ReaderT' with the given environment
-- and returns the final value.
runReader :: forall tag r a . Reader tag r a -> r -> a
runReader = coerce (T.runReader @r @a)
{-# INLINE runReader #-}

instance
    ( T.MonadReader r (trans m) -- FIXME: (forall m . T.MonadReader r (trans m))
                                -- otherwise we can leak a MonadReader instance
                                -- from 'm'
    ) => MonadReader tag r (D.Handler '(READER, tag) trans (m :: * -> *))
  where

    ask = coerce (T.ask @r @(trans m))
    {-# INLINE ask #-}

    local =
      coerce (T.local @r @(trans m) @a) ::
        forall dp a . Local r (D.Handler dp trans m) a
    {-# INLINE local #-}

    reader =
      coerce (T.reader @r @(trans m) @a) ::
        forall dp a . (r -> a) -> D.Handler dp trans m a
    {-# INLINE reader #-}
