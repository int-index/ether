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
    ( ReaderT
    , Reader
    , ReaderT'
    , Reader'
    , runReaderT
    , runReader
    , runReaderT'
    , runReader'
    --
    , MonadReader
    , local
    , ask
    , reader
    --
    , MonadReader'
    , local'
    , ask'
    , reader'
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

    local :: proxy tag -> (r -> r) -> m a -> m a

    ask :: proxy tag -> m r
    ask proxy = reader proxy id

    reader :: proxy tag -> (r -> a) -> m a
    reader proxy f = fmap f (ask proxy)

type MonadReader' r = MonadReader r r

local' :: forall m r a . MonadReader' r m => (r -> r) -> m a -> m a
local' = local (Proxy :: Proxy r)

ask' :: forall m r . MonadReader' r m => m r
ask' = ask (Proxy :: Proxy r)

reader' :: forall m r a . MonadReader' r m => (r -> a) -> m a
reader' = reader (Proxy :: Proxy r)

instance {-# OVERLAPPING #-} Monad m => MonadReader tag r (ReaderT tag r m) where
    ask proxy = etherReaderT proxy return
    local proxy f m = etherReaderT proxy (runReaderT proxy m . f)

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
