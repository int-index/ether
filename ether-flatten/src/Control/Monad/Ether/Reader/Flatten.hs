{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}

module Control.Monad.Ether.Reader.Flatten where

import Data.Functor.Identity
import Control.Monad.Ether.Reader.Class as C
import Control.Monad.Trans.Ether.Dispatch
import qualified Control.Monad.Trans.Reader as T
import Control.Lens
import GHC.Prim (Proxy#)

data K_Flatten t = Flatten [t]

type DispatchFlattenT ts = DispatchT (Flatten ts)

reflatten
  :: forall tagsOld tagsNew m a
   . DispatchFlattenT tagsOld m a
  -> DispatchFlattenT tagsNew m a
reflatten m = dispatchT (runDispatchT m)

type DispatchFlattenReaderT ts r m = DispatchFlattenT ts (T.ReaderT r m)

class HasLens tag outer inner | tag outer -> inner where
  lensOf :: Proxy# tag -> Lens' outer inner

instance (Monad m, HasLens tag outer inner)
      => C.MonadReader tag inner (DispatchFlattenReaderT (tag ': tags) outer m) where
  ask t = dispatchT $ view (lensOf t)
  local t f = dispatchT . T.local (over (lensOf t) f) . runDispatchT

instance {-# OVERLAPPABLE #-}
         (Monad m, C.MonadReader tag inner (DispatchFlattenReaderT tags outer m))
      => C.MonadReader tag inner (DispatchFlattenReaderT (t ': tags) outer m) where
  ask t = reflatten @tags @(t ': tags) (C.ask t)
  local t f = reflatten @tags @(t ': tags) . C.local t f . reflatten @(t ': tags) @tags

runReaderT :: forall tags outer m a . DispatchFlattenReaderT tags outer m a -> outer -> m a
runReaderT m outer = T.runReaderT (runDispatchT m) outer

runReader :: forall tags outer a . DispatchFlattenReaderT tags outer Identity a -> outer -> a
runReader m outer = T.runReader (runDispatchT m) outer
