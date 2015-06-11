{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Ether.Reader
    (
    -- * MonadReader class
      MonadReader(..)
    , asks
    -- * The Reader monad
    , Reader
    , runReader
    -- * The ReaderT monad transformer
    , ReaderT
    , runReaderT
    ) where

import Data.Proxy (Proxy(Proxy))
import Control.Monad.Trans (lift)

import Control.Monad.Trans.Ether.State (StateT, mapStateT)
import Control.Monad.Trans.Ether.Reader

-- for mtl instances
import qualified Control.Monad.Trans.Cont          as Trans        (ContT    , liftLocal)
import qualified Control.Monad.Trans.Except        as Trans        (ExceptT  , mapExceptT)
import qualified Control.Monad.Trans.Identity      as Trans        (IdentityT, mapIdentityT)
import qualified Control.Monad.Trans.List          as Trans        (ListT    , mapListT)
import qualified Control.Monad.Trans.Maybe         as Trans        (MaybeT   , mapMaybeT)
import qualified Control.Monad.Trans.Reader        as Trans        (ReaderT  , mapReaderT)
import qualified Control.Monad.Trans.State.Lazy    as Trans.Lazy   (StateT   , mapStateT)
import qualified Control.Monad.Trans.State.Strict  as Trans.Strict (StateT   , mapStateT)
import qualified Control.Monad.Trans.Writer.Lazy   as Trans.Lazy   (WriterT  , mapWriterT)
import qualified Control.Monad.Trans.Writer.Strict as Trans.Strict (WriterT  , mapWriterT)

class Monad m => MonadReader tag r m | m tag -> r where

    {-# MINIMAL (ask | reader), local #-}

    -- | Retrieves the monad environment.
    ask :: proxy tag -> m r
    ask proxy = reader proxy id

    -- | Executes a computation in a modified environment.
    local
        :: proxy tag
        -> (r -> r)
        -- ^ The function to modify the environment.
        -> m a
        -- ^ @Reader@ to run in the modified environment.
        -> m a

    -- | Retrieves a function of the current environment.
    reader
        :: proxy tag
        -> (r -> a)
        -- ^ The selector function to apply to the environment.
        -> m a
    reader proxy f = fmap f (ask proxy)

-- | Retrieves a function of the current environment.
asks
    :: MonadReader tag r m
    => proxy tag
    -> (r -> a)
    -- ^ The selector function to apply to the environment.
    -> m a
asks = reader

instance {-# OVERLAPPING #-} Monad m => MonadReader tag r (ReaderT tag r m) where
    ask proxy = readerT proxy return
    local proxy f m = readerT proxy (runReaderT proxy m . f)

instance (MonadReader tag r m) => MonadReader tag r (ReaderT tag' r' m) where
    ask proxy = lift (ask proxy)
    local proxy = mapReaderT Proxy . local proxy

-- Instances for other tagged transformers

instance (MonadReader tag r m) => MonadReader tag r (StateT tag' s m) where
    ask proxy = lift (ask proxy)
    local proxy = mapStateT Proxy . local proxy

-- Instances for mtl transformers

instance MonadReader tag r m => MonadReader tag r (Trans.ContT r' m) where
    ask proxy = lift (ask proxy)
    local proxy = Trans.liftLocal (ask proxy) (local proxy)

instance MonadReader tag r m => MonadReader tag r (Trans.ExceptT e m) where
    ask proxy = lift (ask proxy)
    local proxy = Trans.mapExceptT . local proxy

instance MonadReader tag r m => MonadReader tag r (Trans.IdentityT m) where
    ask proxy = lift (ask proxy)
    local proxy = Trans.mapIdentityT . local proxy

instance MonadReader tag r m => MonadReader tag r (Trans.ListT m) where
    ask proxy = lift (ask proxy)
    local proxy = Trans.mapListT . local proxy

instance MonadReader tag r m => MonadReader tag r (Trans.MaybeT m) where
    ask proxy = lift (ask proxy)
    local proxy = Trans.mapMaybeT . local proxy

instance MonadReader tag r m => MonadReader tag r (Trans.ReaderT r' m) where
    ask proxy = lift (ask proxy)
    local proxy = Trans.mapReaderT . local proxy

instance MonadReader tag r m => MonadReader tag r (Trans.Lazy.StateT s m) where
    ask proxy = lift (ask proxy)
    local proxy = Trans.Lazy.mapStateT . local proxy

instance MonadReader tag r m => MonadReader tag r (Trans.Strict.StateT s m) where
    ask proxy = lift (ask proxy)
    local proxy = Trans.Strict.mapStateT . local proxy

instance (Monoid w, MonadReader tag r m) => MonadReader tag r (Trans.Lazy.WriterT w m) where
    ask proxy = lift (ask proxy)
    local proxy = Trans.Lazy.mapWriterT . local proxy

instance (Monoid w, MonadReader tag r m) => MonadReader tag r (Trans.Strict.WriterT w m) where
    ask proxy = lift (ask proxy)
    local proxy = Trans.Strict.mapWriterT . local proxy
