{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Ether.Core
    ( EtherTagless
    , MonadEther
    , EtherTags
    , infer
    ) where

import Data.Proxy (Proxy(Proxy))
import Data.Functor.Identity
import GHC.Exts (Constraint)

import qualified Control.Monad.Trans.Cont          as Trans
import qualified Control.Monad.Trans.Except        as Trans
import qualified Control.Monad.Trans.Identity      as Trans
import qualified Control.Monad.Trans.List          as Trans
import qualified Control.Monad.Trans.Maybe         as Trans
import qualified Control.Monad.Trans.Reader        as Trans
import qualified Control.Monad.Trans.State.Lazy    as Trans.Lazy
import qualified Control.Monad.Trans.State.Strict  as Trans.Strict
import qualified Control.Monad.Trans.Writer.Lazy   as Trans.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Trans.Strict

infer :: (Proxy tag -> a) -> a
infer f = f Proxy

type family OR (a :: Bool) (b :: Bool) :: Bool where
    OR 'True  a = 'True
    OR 'False a = a

type family ELEM (x :: k) (as :: [k]) :: Bool where
    ELEM x (x ': as) = 'True
    ELEM x (a ': as) = ELEM x as
    ELEM x '[] = 'False

type family EtherTagless (tag :: *) (m :: * -> *) :: Constraint where
    EtherTagless tag m = ELEM tag (EtherTags m) ~ 'False

class Monad m => MonadEther m where
    type family EtherTags m :: [*]

instance MonadEther IO where
    type EtherTags IO = '[]

instance MonadEther Identity where
    type EtherTags Identity = '[]

instance Monad m => MonadEther (Trans.ContT r m) where
    type EtherTags (Trans.ContT r m) = EtherTags m

instance Monad m => MonadEther (Trans.ExceptT e m) where
    type EtherTags (Trans.ExceptT e m) = EtherTags m

instance Monad m => MonadEther (Trans.IdentityT m) where
    type EtherTags (Trans.IdentityT m) = EtherTags m

instance Monad m => MonadEther (Trans.ListT m) where
    type EtherTags (Trans.ListT m) = EtherTags m

instance Monad m => MonadEther (Trans.MaybeT m) where
    type EtherTags (Trans.MaybeT m) = EtherTags m

instance Monad m => MonadEther (Trans.ReaderT r m) where
    type EtherTags (Trans.ReaderT r m) = EtherTags m

instance Monad m => MonadEther (Trans.Lazy.StateT s m) where
    type EtherTags (Trans.Lazy.StateT s m) = EtherTags m

instance Monad m => MonadEther (Trans.Strict.StateT s m) where
    type EtherTags (Trans.Strict.StateT s m) = EtherTags m

instance (Monoid w, Monad m) => MonadEther (Trans.Lazy.WriterT w m) where
    type EtherTags (Trans.Lazy.WriterT w m) = EtherTags m

instance (Monoid w, Monad m) => MonadEther (Trans.Strict.WriterT w m) where
    type EtherTags (Trans.Strict.WriterT w m) = EtherTags m
