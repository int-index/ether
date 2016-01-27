{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Ether.TT where

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

import Control.Ether.Util (MonadApplicative)

-- | Tagged monad transformer.
newtype TT tag trans (m :: * -> *) a = TT (trans m a)
  deriving
    ( Generic
    , Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadFix
    , MonadTrans
    , MonadIO
    , MFunctor
    , MMonad
    , MonadThrow
    , MonadCatch
    , MonadMask )

-- | Type-restricted 'coerce'.
pack :: trans m a -> TT tag trans m a
pack = coerce

-- | Type-restricted 'coerce'.
unpack :: TT tag trans m a -> trans m a
unpack = coerce

instance
    ( MB.MonadBase b m
    , MonadTrans trans
    , MonadApplicative (trans m)
    ) => MB.MonadBase b (TT tag trans m)
  where
    liftBase = MB.liftBaseDefault

instance
    ( MC.MonadTransControl trans
    ) => MC.MonadTransControl (TT tag trans)
  where
    type StT (TT tag trans) a = MC.StT trans a
    liftWith = MC.defaultLiftWith pack unpack
    restoreT = MC.defaultRestoreT pack

instance
    ( MC.MonadBaseControl b m
    , MC.MonadTransControl trans
    , MonadApplicative (trans m)
    ) => MC.MonadBaseControl b (TT tag trans m)
  where
    type StM (TT tag trans m) a = MC.ComposeSt trans m a
    liftBaseWith = MC.defaultLiftBaseWith
    restoreM = MC.defaultRestoreM

type instance Lift.StT (TT tag trans) a = Lift.StT trans a

instance Lift.LiftLocal trans => Lift.LiftLocal (TT tag trans) where
  liftLocal = Lift.defaultLiftLocal pack unpack

instance Lift.LiftCatch trans => Lift.LiftCatch (TT tag trans) where
  liftCatch = Lift.defaultLiftCatch pack unpack

instance Lift.LiftListen trans => Lift.LiftListen (TT tag trans) where
  liftListen = Lift.defaultLiftListen pack unpack

instance Lift.LiftPass trans => Lift.LiftPass (TT tag trans) where
  liftPass = Lift.defaultLiftPass pack unpack

instance Lift.LiftCallCC trans => Lift.LiftCallCC (TT tag trans) where
  liftCallCC  = Lift.defaultLiftCallCC  pack unpack
  liftCallCC' = Lift.defaultLiftCallCC' pack unpack

-- Instances for mtl classes

instance
    ( Class.MonadCont m
    , Lift.LiftCallCC trans
    , Monad (trans m)
    ) => Class.MonadCont (TT tag trans m)
  where
    callCC = Lift.liftCallCC' Class.callCC

instance
    ( Class.MonadReader r m
    , Lift.LiftLocal trans
    , Monad (trans m)
    ) => Class.MonadReader r (TT tag trans m)
  where
    ask = lift Class.ask
    local = Lift.liftLocal Class.ask Class.local
    reader = lift . Class.reader

instance
    ( Class.MonadState s m
    , MonadTrans trans
    , Monad (trans m)
    ) => Class.MonadState s (TT tag trans m)
  where
    get = lift Class.get
    put = lift . Class.put
    state = lift . Class.state

instance
    ( Class.MonadWriter w m
    , Lift.LiftListen trans
    , Lift.LiftPass trans
    , Monad (trans m)
    ) => Class.MonadWriter w (TT tag trans m)
  where
    writer = lift . Class.writer
    tell   = lift . Class.tell
    listen = Lift.liftListen Class.listen
    pass   = Lift.liftPass Class.pass

instance
    ( Class.MonadError e m
    , Lift.LiftCatch trans
    , Monad (trans m)
    ) => Class.MonadError e (TT tag trans m)
  where
    throwError = lift . Class.throwError
    catchError = Lift.liftCatch Class.catchError
