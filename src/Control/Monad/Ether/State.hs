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
module Control.Monad.Ether.State
    ( EtherStateT
    , EtherState
    , StateT'
    , State'
    , runEtherStateT
    , runEtherState
    , evalEtherStateT
    , evalEtherState
    , execEtherStateT
    , execEtherState
    , runStateT'
    , runState'
    , evalStateT'
    , evalState'
    , execStateT'
    , execState'
    --
    , MonadEtherState
    , etherGet
    , etherGets
    , etherPut
    , etherState
    , etherModify
    --
    , MonadState'
    , get'
    , gets'
    , put'
    , state'
    , modify'
    ) where

import Data.Proxy (Proxy(Proxy))
import Control.Monad.Trans (lift)

import Control.Monad.Trans.Ether.Reader (EtherReaderT)
import Control.Monad.Trans.Ether.State

-- for mtl instances
import qualified Control.Monad.Trans.Cont          as Trans        (ContT)
import qualified Control.Monad.Trans.Except        as Trans        (ExceptT)
import qualified Control.Monad.Trans.Identity      as Trans        (IdentityT)
import qualified Control.Monad.Trans.List          as Trans        (ListT)
import qualified Control.Monad.Trans.Maybe         as Trans        (MaybeT)
import qualified Control.Monad.Trans.Reader        as Trans        (ReaderT)
import qualified Control.Monad.Trans.State.Lazy    as Trans.Lazy   (StateT)
import qualified Control.Monad.Trans.State.Strict  as Trans.Strict (StateT)
import qualified Control.Monad.Trans.Writer.Lazy   as Trans.Lazy   (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as Trans.Strict (WriterT)

class Monad m => MonadEtherState tag s m | m tag -> s where

    {-# MINIMAL etherState | etherGet, etherPut #-}

    etherGet :: proxy tag -> m s
    etherGet proxy = etherState proxy (\s -> (s, s))

    etherPut :: proxy tag -> s -> m ()
    etherPut proxy s = etherState proxy (\_ -> ((), s))

    etherState :: proxy tag -> (s -> (a, s)) -> m a
    etherState proxy f = do
        s <- etherGet proxy
        let ~(a, s') = f s
        etherPut proxy s'
        return a

etherGets :: MonadEtherState tag s m => proxy tag -> (s -> a) -> m a
etherGets proxy f = fmap f (etherGet proxy)

etherModify :: MonadEtherState tag s m => proxy tag -> (s -> s) -> m ()
etherModify proxy f = etherState proxy $ \ s -> ((), f s)

type MonadState' s = MonadEtherState s s

get' :: forall m s . MonadState' s m => m s
get' = etherGet (Proxy :: Proxy s)

gets' :: forall m s a . MonadState' s m => (s -> a) -> m a
gets' = etherGets (Proxy :: Proxy s)

put' :: forall m s . MonadState' s m => s -> m ()
put' = etherPut (Proxy :: Proxy s)

state' :: forall m s a . MonadState' s m => (s -> (a, s)) -> m a
state' = etherState (Proxy :: Proxy s)

modify' :: forall m s . MonadState' s m => (s -> s) -> m ()
modify' = etherModify (Proxy :: Proxy s)

instance {-# OVERLAPPING #-} Monad m => MonadEtherState tag s (EtherStateT tag s m) where
    etherGet proxy = etherStateT proxy (\s -> return (s, s))
    etherPut proxy s = etherStateT proxy (\_ -> return ((), s))

instance (MonadEtherState tag s m) => MonadEtherState tag s (EtherStateT tag' s' m) where
    etherGet proxy = lift (etherGet proxy)
    etherPut proxy = lift . etherPut proxy
    etherState proxy = lift . etherState proxy

-- Instances for other tagged transformers

instance (MonadEtherState tag s m) => MonadEtherState tag s (EtherReaderT tag' r m) where
    etherGet proxy = lift (etherGet proxy)
    etherPut proxy = lift . etherPut proxy
    etherState proxy = lift . etherState proxy

-- Instances for mtl transformers

instance MonadEtherState tag s m => MonadEtherState tag s (Trans.ContT r m) where
    etherGet proxy = lift (etherGet proxy)
    etherPut proxy = lift . etherPut proxy
    etherState proxy = lift . etherState proxy

instance MonadEtherState tag s m => MonadEtherState tag s (Trans.ExceptT e m) where
    etherGet proxy = lift (etherGet proxy)
    etherPut proxy = lift . etherPut proxy
    etherState proxy = lift . etherState proxy

instance MonadEtherState tag s m => MonadEtherState tag s (Trans.IdentityT m) where
    etherGet proxy = lift (etherGet proxy)
    etherPut proxy = lift . etherPut proxy
    etherState proxy = lift . etherState proxy

instance MonadEtherState tag s m => MonadEtherState tag s (Trans.ListT m) where
    etherGet proxy = lift (etherGet proxy)
    etherPut proxy = lift . etherPut proxy
    etherState proxy = lift . etherState proxy

instance MonadEtherState tag s m => MonadEtherState tag s (Trans.MaybeT m) where
    etherGet proxy = lift (etherGet proxy)
    etherPut proxy = lift . etherPut proxy
    etherState proxy = lift . etherState proxy

instance MonadEtherState tag s m => MonadEtherState tag s (Trans.ReaderT r m) where
    etherGet proxy = lift (etherGet proxy)
    etherPut proxy = lift . etherPut proxy
    etherState proxy = lift . etherState proxy

instance MonadEtherState tag s m => MonadEtherState tag s (Trans.Lazy.StateT s' m) where
    etherGet proxy = lift (etherGet proxy)
    etherPut proxy = lift . etherPut proxy
    etherState proxy = lift . etherState proxy

instance MonadEtherState tag s m => MonadEtherState tag s (Trans.Strict.StateT s' m) where
    etherGet proxy = lift (etherGet proxy)
    etherPut proxy = lift . etherPut proxy
    etherState proxy = lift . etherState proxy

instance (Monoid w, MonadEtherState tag s m) => MonadEtherState tag s (Trans.Lazy.WriterT w m) where
    etherGet proxy = lift (etherGet proxy)
    etherPut proxy = lift . etherPut proxy
    etherState proxy = lift . etherState proxy

instance (Monoid w, MonadEtherState tag s m) => MonadEtherState tag s (Trans.Strict.WriterT w m) where
    etherGet proxy = lift (etherGet proxy)
    etherPut proxy = lift . etherPut proxy
    etherState proxy = lift . etherState proxy
