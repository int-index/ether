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

module Control.Monad.Ether.State.Flatten
  ( runState
  , runStateT
  ) where

import qualified Control.Monad.Trans.Class as Lift

import Data.Functor.Identity
import Control.Monad.Ether.State.Class as C
import Control.Monad.Trans.Ether.Dispatch
import qualified Control.Monad.Trans.State as T
import Control.Lens
import Control.Ether.Flatten

data STATE (ts :: [k])

type StateT ts s = Dispatch (STATE ts) (T.StateT s)

type State ts s = StateT ts s Identity

reflatten
  :: forall tagsOld tagsNew s m a
   . StateT tagsOld s m a
  -> StateT tagsNew s m a
reflatten = repack
{-# INLINE reflatten #-}

instance
    ( Monad m, Monad (trans m)
    , Lift.MonadTrans trans
    , C.MonadState tag s m
    ) => C.MonadState tag s (Dispatch (STATE '[]) trans m)
  where
    get t = Lift.lift (get t)
    put t = Lift.lift . put t
    state t = Lift.lift . state t

instance
    ( Monad m, HasLens tag payload s
    , trans ~ T.StateT payload
    ) => C.MonadState tag s (Dispatch (STATE (tag ': tags)) trans m)
  where
    get t = pack $ use (lensOf t)
    {-# INLINE get #-}
    put t s = pack $ zoom (lensOf t) (T.put s)
    {-# INLINE put #-}

instance {-# OVERLAPPABLE #-}
    ( Monad m
    , C.MonadState tag s (Dispatch (STATE tags) trans m)
    , trans ~ T.StateT payload
    ) => C.MonadState tag s (Dispatch (STATE (t ': tags)) trans m)
  where
    get t = reflatten @tags @(t ': tags) (C.get t)
    {-# INLINE get #-}
    put t = reflatten @tags @(t ': tags) . C.put t
    {-# INLINE put #-}

runStateT
  :: StateT tags (Product tags as) m a
  -> Product tags as
  -> m (a, Product tags as)
runStateT m = T.runStateT (unpack m)

runState
  :: State tags (Product tags as) a
  -> Product tags as
  -> (a, Product tags as)
runState m = T.runState (unpack m)
