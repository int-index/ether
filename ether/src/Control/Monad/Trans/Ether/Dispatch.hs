{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Trans.Ether.Dispatch where

import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (MFunctor, MMonad)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)

import qualified Control.Monad.Base as MB
import qualified Control.Monad.Trans.Control as MC

import qualified Control.Monad.Trans.Lift.StT    as Lift
import qualified Control.Monad.Trans.Lift.Local  as Lift
import qualified Control.Monad.Trans.Lift.Catch  as Lift
import qualified Control.Monad.Trans.Lift.Listen as Lift
import qualified Control.Monad.Trans.Lift.Pass   as Lift
import qualified Control.Monad.Trans.Lift.CallCC as Lift

import qualified Control.Monad.Cont.Class    as Class
import qualified Control.Monad.Reader.Class  as Class
import qualified Control.Monad.State.Class   as Class
import qualified Control.Monad.Writer.Class  as Class
import qualified Control.Monad.Error.Class   as Class

import GHC.Generics (Generic)
import Data.Coerce (coerce)

newtype Dispatch dp trans m a = Dispatch (trans m a)
  deriving
    ( Generic
    , Functor, Applicative, Alternative, Monad, MonadPlus
    , MonadFix, MonadTrans, MonadIO, MFunctor, MMonad
    , MonadThrow, MonadCatch, MonadMask )

pack :: forall dp trans m a . trans m a -> Dispatch dp trans m a
pack = coerce
{-# INLINE pack #-}

unpack :: forall dp trans m a . Dispatch dp trans m a -> trans m a
unpack = coerce
{-# INLINE unpack #-}

repack :: forall dp dp' trans m a . Dispatch dp trans m a -> Dispatch dp' trans m a
repack = coerce
{-# INLINE repack #-}

instance
    ( MB.MonadBase b m
    , MonadTrans trans
    , Monad (trans m)
    ) => MB.MonadBase b (Dispatch dp trans m)
  where
    liftBase = MB.liftBaseDefault

instance
    ( MC.MonadTransControl trans
    ) => MC.MonadTransControl (Dispatch dp trans)
  where
    type StT (Dispatch dp trans) a = MC.StT trans a
    liftWith = MC.defaultLiftWith pack unpack
    restoreT = MC.defaultRestoreT pack

instance
    ( MC.MonadBaseControl b m
    , MC.MonadTransControl trans
    , Monad (trans m)
    ) => MC.MonadBaseControl b (Dispatch dp trans m)
  where
    type StM (Dispatch dp trans m) a = MC.ComposeSt trans m a
    liftBaseWith = MC.defaultLiftBaseWith
    restoreM = MC.defaultRestoreM

type instance Lift.StT (Dispatch dp trans) a = Lift.StT trans a

instance Lift.LiftLocal trans => Lift.LiftLocal (Dispatch dp trans) where
  liftLocal = Lift.defaultLiftLocal pack unpack

instance Lift.LiftCatch trans => Lift.LiftCatch (Dispatch dp trans) where
  liftCatch = Lift.defaultLiftCatch pack unpack

instance Lift.LiftListen trans => Lift.LiftListen (Dispatch dp trans) where
  liftListen = Lift.defaultLiftListen pack unpack

instance Lift.LiftPass trans => Lift.LiftPass (Dispatch dp trans) where
  liftPass = Lift.defaultLiftPass pack unpack

instance Lift.LiftCallCC trans => Lift.LiftCallCC (Dispatch dp trans) where
  liftCallCC  = Lift.defaultLiftCallCC  pack unpack
  liftCallCC' = Lift.defaultLiftCallCC' pack unpack


-- Instances for mtl classes

instance {-# OVERLAPPABLE #-}
    ( Class.MonadCont m
    , Lift.LiftCallCC trans
    , Monad (trans m)
    ) => Class.MonadCont (Dispatch dp trans m)
  where
    callCC = Lift.liftCallCC' Class.callCC

instance {-# OVERLAPPABLE #-}
    ( Class.MonadReader r m
    , Lift.LiftLocal trans
    , Monad (trans m)
    ) => Class.MonadReader r (Dispatch dp trans m)
  where
    ask = lift Class.ask
    local = Lift.liftLocal Class.ask Class.local
    reader = lift . Class.reader

instance {-# OVERLAPPABLE #-}
    ( Class.MonadState s m
    , MonadTrans trans
    , Monad (trans m)
    ) => Class.MonadState s (Dispatch dp trans m)
  where
    get = lift Class.get
    put = lift . Class.put
    state = lift . Class.state

instance {-# OVERLAPPABLE #-}
    ( Class.MonadWriter w m
    , Lift.LiftListen trans
    , Lift.LiftPass trans
    , Monad (trans m)
    ) => Class.MonadWriter w (Dispatch dp trans m)
  where
    writer = lift . Class.writer
    tell   = lift . Class.tell
    listen = Lift.liftListen Class.listen
    pass   = Lift.liftPass Class.pass

instance {-# OVERLAPPABLE #-}
    ( Class.MonadError e m
    , Lift.LiftCatch trans
    , Monad (trans m)
    ) => Class.MonadError e (Dispatch dp trans m)
  where
    throwError = lift . Class.throwError
    catchError = Lift.liftCatch Class.catchError
