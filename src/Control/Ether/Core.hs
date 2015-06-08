{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Ether.Core
    ( EtherTagless
    , EtherTags
    ) where

import Data.Proxy (Proxy)
import Data.Functor.Identity (Identity)
import Data.Monoid (First, Last)
import qualified Control.Monad.ST.Strict as Strict (ST)
import qualified Control.Monad.ST.Lazy   as Lazy   (ST)
import GHC.Conc (STM)
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

type family OR (a :: Bool) (b :: Bool) :: Bool where
    OR 'True  a = 'True
    OR 'False a = a

type family ELEM (x :: k) (as :: [k]) :: Bool where
    ELEM x (x ': as) = 'True
    ELEM x (a ': as) = ELEM x as
    ELEM x '[] = 'False

type family EtherTagless (tag :: *) (m :: * -> *) :: Constraint where
    EtherTagless tag m = ELEM tag (EtherTags m) ~ 'False

type family EtherTags (m :: * -> *) :: [*]

type instance EtherTags IO = '[]
type instance EtherTags Identity = '[]
type instance EtherTags [] = '[]
type instance EtherTags Maybe = '[]
type instance EtherTags Last = '[]
type instance EtherTags First = '[]
type instance EtherTags ((->) r) = '[]
type instance EtherTags STM = '[]
type instance EtherTags (Either e) = '[]
type instance EtherTags Proxy = '[]
type instance EtherTags (Strict.ST s) = '[]
type instance EtherTags (Lazy.ST s) = '[]

type instance EtherTags (Trans.ContT r m) = EtherTags m
type instance EtherTags (Trans.ExceptT e m) = EtherTags m
type instance EtherTags (Trans.IdentityT m) = EtherTags m
type instance EtherTags (Trans.ListT m) = EtherTags m
type instance EtherTags (Trans.MaybeT m) = EtherTags m
type instance EtherTags (Trans.ReaderT r m) = EtherTags m
type instance EtherTags (Trans.Lazy.StateT s m) = EtherTags m
type instance EtherTags (Trans.Strict.StateT s m) = EtherTags m
type instance EtherTags (Trans.Lazy.WriterT w m) = EtherTags m
type instance EtherTags (Trans.Strict.WriterT w m) = EtherTags m
