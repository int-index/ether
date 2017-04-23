module Control.Monad.Trans.Ether.Handler
  ( Handler(..)
  ) where

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

newtype Handler eff trans m a = Handler (trans m a)
  deriving
    ( Generic
    , Functor, Applicative, Alternative, Monad, MonadPlus
    , MonadFix, MonadTrans, MonadIO, MFunctor, MMonad
    , MonadThrow, MonadCatch, MonadMask )

type Pack eff trans m a = trans m a -> Handler eff trans m a

type Unpack eff trans m a = Handler eff trans m a -> trans m a

instance
    ( MB.MonadBase b (trans m)
    ) => MB.MonadBase b (Handler eff trans m)
  where
    liftBase =
      (coerce :: forall a .
        (b a -> trans m a) ->
        (b a -> Handler eff trans m a))
      MB.liftBase

instance
    ( MC.MonadTransControl trans
    ) => MC.MonadTransControl (Handler eff trans)
  where
    type StT (Handler eff trans) a = MC.StT trans a
    liftWith = MC.defaultLiftWith
      (coerce :: Pack eff trans m a)
      (coerce :: Unpack eff trans m a)
    restoreT = MC.defaultRestoreT
      (coerce :: Pack eff trans m a)

type LiftBaseWith b m a = (MC.RunInBase m b -> b a) -> m a

newtype LiftBaseWith' b m a = LBW { unLBW :: LiftBaseWith b m a }

coerceLiftBaseWith ::
  LiftBaseWith b            (trans m) a ->
  LiftBaseWith b (Handler eff trans m) a
coerceLiftBaseWith lbw =
  unLBW (coerce (LBW lbw))
{-# INLINE coerceLiftBaseWith #-}

instance
    ( MC.MonadBaseControl b (trans m)
    ) => MC.MonadBaseControl b (Handler eff trans m)
  where
    type StM (Handler eff trans m) a = MC.StM (trans m) a

    liftBaseWith = coerceLiftBaseWith MC.liftBaseWith
    {-# INLINE liftBaseWith #-}

    restoreM =
      (coerce :: forall a .
        (MC.StM (trans m) a ->            trans m a) ->
        (MC.StM (trans m) a -> Handler eff trans m a))
      MC.restoreM
    {-# INLINE restoreM #-}

type instance Lift.StT (Handler eff trans) a = Lift.StT trans a

instance Lift.LiftLocal trans => Lift.LiftLocal (Handler eff trans) where
  liftLocal =
    Lift.defaultLiftLocal
      (coerce :: Pack eff trans m a)
      (coerce :: Unpack eff trans m a)

instance Lift.LiftCatch trans => Lift.LiftCatch (Handler eff trans) where
  liftCatch =
    Lift.defaultLiftCatch
      (coerce :: Pack eff trans m a)
      (coerce :: Unpack eff trans m a)

instance Lift.LiftListen trans => Lift.LiftListen (Handler eff trans) where
  liftListen =
    Lift.defaultLiftListen
      (coerce :: Pack eff trans m a)
      (coerce :: Unpack eff trans m a)

instance Lift.LiftPass trans => Lift.LiftPass (Handler eff trans) where
  liftPass =
    Lift.defaultLiftPass
      (coerce :: Pack eff trans m a)
      (coerce :: Unpack eff trans m a)

instance Lift.LiftCallCC trans => Lift.LiftCallCC (Handler eff trans) where
  liftCallCC  =
    Lift.defaultLiftCallCC
      (coerce :: Pack eff trans m a)
      (coerce :: Unpack eff trans m a)
  liftCallCC' =
    Lift.defaultLiftCallCC'
      (coerce :: Pack eff trans m a)
      (coerce :: Unpack eff trans m a)


-- Instances for mtl classes

instance {-# OVERLAPPABLE #-}
    ( Class.MonadCont m
    , Lift.LiftCallCC trans
    , Monad (trans m)
    ) => Class.MonadCont (Handler eff trans m)
  where
    callCC = Lift.liftCallCC' Class.callCC

instance {-# OVERLAPPABLE #-}
    ( Class.MonadReader r m
    , Lift.LiftLocal trans
    , Monad (trans m)
    ) => Class.MonadReader r (Handler eff trans m)
  where
    ask = lift Class.ask
    local = Lift.liftLocal Class.ask Class.local
    reader = lift . Class.reader

instance {-# OVERLAPPABLE #-}
    ( Class.MonadState s m
    , MonadTrans trans
    , Monad (trans m)
    ) => Class.MonadState s (Handler eff trans m)
  where
    get = lift Class.get
    put = lift . Class.put
    state = lift . Class.state

instance {-# OVERLAPPABLE #-}
    ( Class.MonadWriter w m
    , Lift.LiftListen trans
    , Lift.LiftPass trans
    , Monad (trans m)
    ) => Class.MonadWriter w (Handler eff trans m)
  where
    writer = lift . Class.writer
    tell   = lift . Class.tell
    listen = Lift.liftListen Class.listen
    pass   = Lift.liftPass Class.pass

instance {-# OVERLAPPABLE #-}
    ( Class.MonadError e m
    , Lift.LiftCatch trans
    , Monad (trans m)
    ) => Class.MonadError e (Handler eff trans m)
  where
    throwError = lift . Class.throwError
    catchError = Lift.liftCatch Class.catchError
