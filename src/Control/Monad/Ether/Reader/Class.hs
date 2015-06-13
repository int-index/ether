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

-- | See "Control.Monad.Reader.Class".

module Control.Monad.Ether.Reader.Class
    ( MonadReader(..)
    , asks
    ) where

import Data.Proxy (Proxy(Proxy))
import Control.Monad.Trans (lift)

import Control.Monad.Trans.Ether.Writer (WriterT, mapWriterT)
import Control.Monad.Trans.Ether.Except (ExceptT, mapExceptT)
import Control.Monad.Trans.Ether.Reader hiding (reader, ask, local)
import qualified Control.Monad.Trans.Ether.Reader as R
import qualified Control.Monad.Trans.Ether.State.Lazy   as S.L
import qualified Control.Monad.Trans.Ether.State.Strict as S.S

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

-- | See 'Control.Monad.Reader.MonadReader'.
class Monad m => MonadReader tag r m | m tag -> r where

    {-# MINIMAL (ask | reader), local #-}

    -- | Retrieves the monad environment.
    ask :: proxy tag -> m r
    ask t = reader t id

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
    reader t f = fmap f (ask t)

-- | Retrieves a function of the current environment.
asks
    :: MonadReader tag r m
    => proxy tag
    -> (r -> a)
    -- ^ The selector function to apply to the environment.
    -> m a
asks = reader

instance {-# OVERLAPPING #-} Monad m => MonadReader tag r (ReaderT tag r m) where
    ask = R.ask
    local = R.local
    reader = R.reader

instance MonadReader tag r m => MonadReader tag r (ReaderT tag' r' m) where
    ask t = lift (ask t)
    local t = mapReaderT Proxy . local t

-- Instances for other tagged transformers

instance (Monoid w, MonadReader tag r m) => MonadReader tag r (WriterT tag' w m) where
    ask t = lift (ask t)
    local t = mapWriterT Proxy . local t

instance (MonadReader tag r m) => MonadReader tag r (S.L.StateT tag' s m) where
    ask t = lift (ask t)
    local t = S.L.mapStateT Proxy . local t

instance (MonadReader tag r m) => MonadReader tag r (S.S.StateT tag' s m) where
    ask t = lift (ask t)
    local t = S.S.mapStateT Proxy . local t

instance (MonadReader tag r m) => MonadReader tag r (ExceptT tag' e m) where
    ask t = lift (ask t)
    local t = mapExceptT Proxy . local t

-- Instances for mtl transformers

instance MonadReader tag r m => MonadReader tag r (Trans.ContT r' m) where
    ask t = lift (ask t)
    local t = Trans.liftLocal (ask t) (local t)

instance MonadReader tag r m => MonadReader tag r (Trans.ExceptT e m) where
    ask t = lift (ask t)
    local t = Trans.mapExceptT . local t

instance MonadReader tag r m => MonadReader tag r (Trans.IdentityT m) where
    ask t = lift (ask t)
    local t = Trans.mapIdentityT . local t

instance MonadReader tag r m => MonadReader tag r (Trans.ListT m) where
    ask t = lift (ask t)
    local t = Trans.mapListT . local t

instance MonadReader tag r m => MonadReader tag r (Trans.MaybeT m) where
    ask t = lift (ask t)
    local t = Trans.mapMaybeT . local t

instance MonadReader tag r m => MonadReader tag r (Trans.ReaderT r' m) where
    ask t = lift (ask t)
    local t = Trans.mapReaderT . local t

instance MonadReader tag r m => MonadReader tag r (Trans.Lazy.StateT s m) where
    ask t = lift (ask t)
    local t = Trans.Lazy.mapStateT . local t

instance MonadReader tag r m => MonadReader tag r (Trans.Strict.StateT s m) where
    ask t = lift (ask t)
    local t = Trans.Strict.mapStateT . local t

instance (Monoid w, MonadReader tag r m) => MonadReader tag r (Trans.Lazy.WriterT w m) where
    ask t = lift (ask t)
    local t = Trans.Lazy.mapWriterT . local t

instance (Monoid w, MonadReader tag r m) => MonadReader tag r (Trans.Strict.WriterT w m) where
    ask t = lift (ask t)
    local t = Trans.Strict.mapWriterT . local t
