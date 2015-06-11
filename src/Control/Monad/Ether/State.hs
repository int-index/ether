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
    ( StateT
    , State
    , runStateT
    , runState
    , evalStateT
    , evalState
    , execStateT
    , execState
    --
    , MonadState
    , get
    , gets
    , put
    , state
    , modify
    ) where

import Control.Monad.Trans (lift)

import Control.Monad.Trans.Ether.Reader (ReaderT)
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

class Monad m => MonadState tag s m | m tag -> s where

    {-# MINIMAL state | get, put #-}

    get :: proxy tag -> m s
    get proxy = state proxy (\s -> (s, s))

    put :: proxy tag -> s -> m ()
    put proxy s = state proxy (\_ -> ((), s))

    state :: proxy tag -> (s -> (a, s)) -> m a
    state proxy f = do
        s <- get proxy
        let ~(a, s') = f s
        put proxy s'
        return a

gets :: MonadState tag s m => proxy tag -> (s -> a) -> m a
gets proxy f = fmap f (get proxy)

modify :: MonadState tag s m => proxy tag -> (s -> s) -> m ()
modify proxy f = state proxy $ \ s -> ((), f s)

instance {-# OVERLAPPING #-} Monad m => MonadState tag s (StateT tag s m) where
    get proxy = stateT proxy (\s -> return (s, s))
    put proxy s = stateT proxy (\_ -> return ((), s))

instance (MonadState tag s m) => MonadState tag s (StateT tag' s' m) where
    get proxy = lift (get proxy)
    put proxy = lift . put proxy
    state proxy = lift . state proxy

-- Instances for other tagged transformers

instance (MonadState tag s m) => MonadState tag s (ReaderT tag' r m) where
    get proxy = lift (get proxy)
    put proxy = lift . put proxy
    state proxy = lift . state proxy

-- Instances for mtl transformers

instance MonadState tag s m => MonadState tag s (Trans.ContT r m) where
    get proxy = lift (get proxy)
    put proxy = lift . put proxy
    state proxy = lift . state proxy

instance MonadState tag s m => MonadState tag s (Trans.ExceptT e m) where
    get proxy = lift (get proxy)
    put proxy = lift . put proxy
    state proxy = lift . state proxy

instance MonadState tag s m => MonadState tag s (Trans.IdentityT m) where
    get proxy = lift (get proxy)
    put proxy = lift . put proxy
    state proxy = lift . state proxy

instance MonadState tag s m => MonadState tag s (Trans.ListT m) where
    get proxy = lift (get proxy)
    put proxy = lift . put proxy
    state proxy = lift . state proxy

instance MonadState tag s m => MonadState tag s (Trans.MaybeT m) where
    get proxy = lift (get proxy)
    put proxy = lift . put proxy
    state proxy = lift . state proxy

instance MonadState tag s m => MonadState tag s (Trans.ReaderT r m) where
    get proxy = lift (get proxy)
    put proxy = lift . put proxy
    state proxy = lift . state proxy

instance MonadState tag s m => MonadState tag s (Trans.Lazy.StateT s' m) where
    get proxy = lift (get proxy)
    put proxy = lift . put proxy
    state proxy = lift . state proxy

instance MonadState tag s m => MonadState tag s (Trans.Strict.StateT s' m) where
    get proxy = lift (get proxy)
    put proxy = lift . put proxy
    state proxy = lift . state proxy

instance (Monoid w, MonadState tag s m) => MonadState tag s (Trans.Lazy.WriterT w m) where
    get proxy = lift (get proxy)
    put proxy = lift . put proxy
    state proxy = lift . state proxy

instance (Monoid w, MonadState tag s m) => MonadState tag s (Trans.Strict.WriterT w m) where
    get proxy = lift (get proxy)
    put proxy = lift . put proxy
    state proxy = lift . state proxy
