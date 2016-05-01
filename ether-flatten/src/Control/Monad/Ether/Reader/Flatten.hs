{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}

module Control.Monad.Ether.Reader.Flatten where

import qualified Control.Monad.Trans.Lift.Local  as Lift

import Data.Functor.Identity
import Control.Monad.Ether.Reader.Class as C
import Control.Monad.Trans.Ether.Dispatch
import qualified Control.Monad.Trans.Reader as T
import Control.Lens
import GHC.Prim (Proxy#)

class HasLens tag outer inner | tag outer -> inner where
  lensOf :: Proxy# tag -> Lens' outer inner

data Payload (ts :: [k]) (as :: [*]) where
  Nil :: Payload '[] '[]
  (:&) :: a -> Payload tags as -> Payload (tag ': tags) (a ': as)

infixr :&

load
  :: forall tag a tags as
   . a
  -> Payload tags as
  -> Payload (tag ': tags) (a ': as)
load = (:&)

productHead :: Lens' (Payload (tag ': tags) (a ': as)) a
productHead = lens (\(a :& _) -> a) (\(_ :& as) a -> a :& as)

productTail :: Lens' (Payload (t ': tags) (b ': as)) (Payload tags as)
productTail = lens (\(_ :& as) -> as) (\(a :& _) as -> a :& as)

instance a ~ b => HasLens tag (Payload (tag ': tags) (a ': as)) b where
  lensOf _ = productHead

instance {-# OVERLAPPABLE #-}
         (HasLens tag (Payload tags as) a)
      => HasLens tag (Payload (t ': tags) (b ': as)) a where
  lensOf t = productTail . lensOf t

data K_Flatten t = Flatten [t]

type DispatchFlattenT ts = DispatchT (Flatten ts)

reflatten
  :: forall tagsOld tagsNew m a
   . DispatchFlattenT tagsOld m a
  -> DispatchFlattenT tagsNew m a
reflatten m = dispatchT (runDispatchT m)

type DispatchFlattenReaderT ts r m = DispatchFlattenT ts (T.ReaderT r m)

instance (Monad m, C.MonadReader tag r m)
      => C.MonadReader tag r (DispatchFlattenReaderT '[] payload m) where
  ask t = Lift.lift (ask t)
  local t = Lift.liftLocal (ask t) (local t)

instance (Monad m, HasLens tag payload r)
      => C.MonadReader tag r (DispatchFlattenReaderT (tag ': tags) payload m) where
  ask t = dispatchT $ view (lensOf t)
  local t f = dispatchT . T.local (over (lensOf t) f) . runDispatchT

instance {-# OVERLAPPABLE #-}
         (Monad m, C.MonadReader tag r (DispatchFlattenReaderT tags payload m))
      => C.MonadReader tag r (DispatchFlattenReaderT (t ': tags) payload m) where
  ask t = reflatten @tags @(t ': tags) (C.ask t)
  local t f = reflatten @tags @(t ': tags) . C.local t f . reflatten @(t ': tags) @tags

runReaderT
  :: DispatchFlattenReaderT tags (Payload tags as) m a
  -> (Payload '[] '[] -> Payload tags as)
  -> m a
runReaderT m f = T.runReaderT (runDispatchT m) (f Nil)

runReader
  :: DispatchFlattenReaderT tags (Payload tags as) Identity a
  -> (Payload '[] '[] -> Payload tags as)
  -> a
runReader m f = T.runReader (runDispatchT m) (f Nil)
