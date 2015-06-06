{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Ether.Reader
    ( module Control.Monad.Ether.Reader
    , module Control.Monad.Trans.Ether.Reader
    ) where

import Data.Proxy (Proxy(Proxy))
import Control.Monad.Trans (lift)

import Control.Ether.Core
import Control.Monad.Trans.Ether.Reader

-- for mtl instances
import qualified Control.Monad.Trans.Cont          as Trans        (ContT    , liftLocal)
import qualified Control.Monad.Trans.Except        as Trans        (ExceptT  , mapExceptT)
import qualified Control.Monad.Trans.Identity      as Trans        (IdentityT, mapIdentityT)
import qualified Control.Monad.Trans.List          as Trans        (ListT    , mapListT)
import qualified Control.Monad.Trans.Maybe         as Trans        (MaybeT   , mapMaybeT)
import qualified Control.Monad.Trans.State.Lazy    as Trans.Lazy   (StateT   , mapStateT)
import qualified Control.Monad.Trans.State.Strict  as Trans.Strict (StateT   , mapStateT)
import qualified Control.Monad.Trans.Writer.Lazy   as Trans.Lazy   (WriterT  , mapWriterT)
import qualified Control.Monad.Trans.Writer.Strict as Trans.Strict (WriterT  , mapWriterT)

class MonadEther m => MonadEtherReader tag m where

    etherLocal :: proxy tag -> (EtherData tag -> EtherData tag) -> m a -> m a

    etherAsk :: proxy tag -> m (EtherData tag)
    etherAsk proxy = etherReader proxy id

    etherReader :: proxy tag -> (EtherData tag -> a) -> m a
    etherReader proxy f = fmap f (etherAsk proxy)

instance {-# OVERLAPPING #-} (Monad m, EtherTagless tag m)
  => MonadEtherReader tag (EtherReaderT tag m) where
    etherAsk proxy = etherReaderT proxy return
    etherLocal proxy f m = etherReaderT proxy (runEtherReaderT proxy m . f)

instance MonadEtherReader tag m => MonadEtherReader tag (EtherReaderT tag' m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = mapEtherReaderT Proxy . etherLocal proxy

-- Instances for mtl transformers

instance MonadEtherReader tag m => MonadEtherReader tag (Trans.ContT r m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = Trans.liftLocal (etherAsk proxy) (etherLocal proxy)

instance MonadEtherReader tag m => MonadEtherReader tag (Trans.ExceptT e m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = Trans.mapExceptT . etherLocal proxy

instance MonadEtherReader tag m => MonadEtherReader tag (Trans.IdentityT m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = Trans.mapIdentityT . etherLocal proxy

instance MonadEtherReader tag m => MonadEtherReader tag (Trans.ListT m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = Trans.mapListT . etherLocal proxy

instance MonadEtherReader tag m => MonadEtherReader tag (Trans.MaybeT m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = Trans.mapMaybeT . etherLocal proxy

instance MonadEtherReader tag m => MonadEtherReader tag (Trans.Lazy.StateT s m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = Trans.Lazy.mapStateT . etherLocal proxy

instance MonadEtherReader tag m => MonadEtherReader tag (Trans.Strict.StateT s m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = Trans.Strict.mapStateT . etherLocal proxy

instance (Monoid w, MonadEtherReader tag m) => MonadEtherReader tag (Trans.Lazy.WriterT w m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = Trans.Lazy.mapWriterT . etherLocal proxy

instance (Monoid w, MonadEtherReader tag m) => MonadEtherReader tag (Trans.Strict.WriterT w m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = Trans.Strict.mapWriterT . etherLocal proxy
