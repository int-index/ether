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

module Control.Monad.Ether.Reader.Flatten
  ( runReader
  , runReaderT
  ) where

import qualified Control.Monad.Trans.Lift.Local  as Lift

import Data.Functor.Identity
import Control.Monad.Ether.Reader.Class as C
import Control.Monad.Trans.Ether.Dispatch
import qualified Control.Monad.Trans.Reader as T
import Control.Lens
import Control.Ether.Flatten

data K_Flatten t = Flatten [t]

type DispatchFlattenT ts = DispatchT (Flatten ts)

reflatten
  :: forall tagsOld tagsNew m a
   . DispatchFlattenT tagsOld m a
  -> DispatchFlattenT tagsNew m a
reflatten = redispatch
{-# INLINE reflatten #-}

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
  :: DispatchFlattenReaderT tags (Product tags as) m a
  -> Product tags as
  -> m a
runReaderT m = T.runReaderT (runDispatchT m)

runReader
  :: DispatchFlattenReaderT tags (Product tags as) Identity a
  -> Product tags as
  -> a
runReader m = T.runReader (runDispatchT m)
