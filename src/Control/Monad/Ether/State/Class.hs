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
module Control.Monad.Ether.State.Class
    ( MonadState(..)
    , modify
    , gets
    ) where

import Control.Monad.Trans (lift)

import Control.Monad.Trans.Ether.Reader (ReaderT)
import Control.Monad.Trans.Ether.Writer (WriterT)
import Control.Monad.Trans.Ether.Except (ExceptT)
import Control.Monad.Trans.Ether.State hiding (state, get, put)
import qualified Control.Monad.Trans.Ether.State as S

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

    -- | Return the state from the internals of the monad.
    get :: proxy tag -> m s
    get t = state t (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: proxy tag -> s -> m ()
    put t s = state t (\_ -> ((), s))

    -- | Embed a simple state action into the monad.
    state :: proxy tag -> (s -> (a, s)) -> m a
    state t f = do
        s <- get t
        let ~(a, s') = f s
        put t s'
        return a

-- | Modifies the state inside a state monad.
modify :: MonadState tag s m => proxy tag -> (s -> s) -> m ()
modify t f = state t (\s -> ((), f s))

-- | Gets specific component of the state, using a projection function supplied.
gets :: MonadState tag s m => proxy tag -> (s -> a) -> m a
gets t f = fmap f (get t)

instance {-# OVERLAPPING #-} Monad m => MonadState tag s (StateT tag s m) where
    get = S.get
    put = S.put
    state = S.state

instance MonadState tag s m => MonadState tag s (StateT tag' s' m) where
    get t = lift (get t)
    put t = lift . put t
    state t = lift . state t

-- Instances for other tagged transformers

instance (MonadState tag s m) => MonadState tag s (ReaderT tag' r m) where
    get t = lift (get t)
    put t = lift . put t
    state t = lift . state t

instance (Monoid w, MonadState tag s m) => MonadState tag s (WriterT tag' w m) where
    get t = lift (get t)
    put t = lift . put t
    state t = lift . state t

instance (MonadState tag s m) => MonadState tag s (ExceptT tag' e m) where
    get t = lift (get t)
    put t = lift . put t
    state t = lift . state t

-- Instances for mtl transformers

instance MonadState tag s m => MonadState tag s (Trans.ContT r m) where
    get t = lift (get t)
    put t = lift . put t
    state t = lift . state t

instance MonadState tag s m => MonadState tag s (Trans.ExceptT e m) where
    get t = lift (get t)
    put t = lift . put t
    state t = lift . state t

instance MonadState tag s m => MonadState tag s (Trans.IdentityT m) where
    get t = lift (get t)
    put t = lift . put t
    state t = lift . state t

instance MonadState tag s m => MonadState tag s (Trans.ListT m) where
    get t = lift (get t)
    put t = lift . put t
    state t = lift . state t

instance MonadState tag s m => MonadState tag s (Trans.MaybeT m) where
    get t = lift (get t)
    put t = lift . put t
    state t = lift . state t

instance MonadState tag s m => MonadState tag s (Trans.ReaderT r m) where
    get t = lift (get t)
    put t = lift . put t
    state t = lift . state t

instance MonadState tag s m => MonadState tag s (Trans.Lazy.StateT s' m) where
    get t = lift (get t)
    put t = lift . put t
    state t = lift . state t

instance MonadState tag s m => MonadState tag s (Trans.Strict.StateT s' m) where
    get t = lift (get t)
    put t = lift . put t
    state t = lift . state t

instance (Monoid w, MonadState tag s m) => MonadState tag s (Trans.Lazy.WriterT w m) where
    get t = lift (get t)
    put t = lift . put t
    state t = lift . state t

instance (Monoid w, MonadState tag s m) => MonadState tag s (Trans.Strict.WriterT w m) where
    get t = lift (get t)
    put t = lift . put t
    state t = lift . state t
