{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Ether.Reader
    ( module Control.Monad.Ether.Reader
    , module Control.Monad.Trans.Ether.Reader
    ) where

import Data.Proxy (Proxy(Proxy))
import Control.Monad.Trans (lift)

import Control.Ether.Core (EtherData)
import Control.Monad.Trans.Ether.Reader

-- for mtl instances
import qualified Control.Monad.Trans.Cont          as Cont          (ContT    , liftLocal)
import qualified Control.Monad.Trans.Except        as Except        (ExceptT  , mapExceptT)
import qualified Control.Monad.Trans.Identity      as Identity      (IdentityT, mapIdentityT)
import qualified Control.Monad.Trans.List          as List          (ListT    , mapListT)
import qualified Control.Monad.Trans.Maybe         as Maybe         (MaybeT   , mapMaybeT)
import qualified Control.Monad.Trans.State.Lazy    as State.Lazy    (StateT   , mapStateT)
import qualified Control.Monad.Trans.State.Strict  as State.Strict  (StateT   , mapStateT)
import qualified Control.Monad.Trans.Writer.Lazy   as Writer.Lazy   (WriterT  , mapWriterT)
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict (WriterT  , mapWriterT)

class Monad m => MonadEtherReader tag m where

    etherLocal :: proxy tag -> (EtherData tag -> EtherData tag) -> m a -> m a

    etherAsk :: proxy tag -> m (EtherData tag)
    etherAsk proxy = etherReader proxy id

    etherReader :: proxy tag -> (EtherData tag -> a) -> m a
    etherReader proxy f = fmap f (etherAsk proxy)

instance {-# OVERLAPS #-} Monad m => MonadEtherReader tag (EtherReaderT tag m) where
    etherAsk proxy = etherReaderT proxy return
    etherLocal proxy f m = etherReaderT proxy (runEtherReaderT proxy m . f)

instance MonadEtherReader tag m => MonadEtherReader tag (EtherReaderT tag' m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = mapEtherReaderT Proxy . etherLocal proxy

-- Instances for mtl transformers

instance MonadEtherReader tag m => MonadEtherReader tag (Cont.ContT r m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = Cont.liftLocal (etherAsk proxy) (etherLocal proxy)

instance MonadEtherReader tag m => MonadEtherReader tag (Except.ExceptT e m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = Except.mapExceptT . etherLocal proxy

instance MonadEtherReader tag m => MonadEtherReader tag (Identity.IdentityT m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = Identity.mapIdentityT . etherLocal proxy

instance MonadEtherReader tag m => MonadEtherReader tag (List.ListT m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = List.mapListT . etherLocal proxy

instance MonadEtherReader tag m => MonadEtherReader tag (Maybe.MaybeT m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = Maybe.mapMaybeT . etherLocal proxy

instance MonadEtherReader tag m => MonadEtherReader tag (State.Lazy.StateT s m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = State.Lazy.mapStateT . etherLocal proxy

instance MonadEtherReader tag m => MonadEtherReader tag (State.Strict.StateT s m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = State.Strict.mapStateT . etherLocal proxy

instance (Monoid w, MonadEtherReader tag m) => MonadEtherReader tag (Writer.Lazy.WriterT w m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = Writer.Lazy.mapWriterT . etherLocal proxy

instance (Monoid w, MonadEtherReader tag m) => MonadEtherReader tag (Writer.Strict.WriterT w m) where
    etherAsk proxy = lift (etherAsk proxy)
    etherLocal proxy = Writer.Strict.mapWriterT . etherLocal proxy
