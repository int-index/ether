{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | See "Control.Monad.Trans.State.Strict".

module Control.Monad.Trans.Ether.State.Strict
    (
    -- * The State monad
      State
    , state
    , runState
    , evalState
    , execState
    -- * The StateT monad transformer
    , StateT
    , stateT
    , runStateT
    , evalStateT
    , execStateT
    -- * State operations
    , get
    , put
    -- * Newtype operations
    , pack
    , unpack
    ) where

import Data.Functor.Identity (Identity(..))
import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (MFunctor)
import GHC.Generics (Generic)
import Data.Coerce (coerce)

import qualified Control.Monad.Base as MB
import qualified Control.Monad.Trans.Control as MC
import qualified Control.Monad.Trans.State.Strict as Trans

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


-- | The parametrizable state monad.
--
-- Computations have access to a mutable state.
--
-- The 'return' function leaves the state unchanged, while '>>=' uses
-- the final state of the first computation as the initial state of the second.
type State tag r = StateT tag r Identity

-- | The state monad transformer.
--
-- The 'return' function leaves the state unchanged, while '>>=' uses
-- the final state of the first computation as the initial state of the second.
newtype StateT tag s m a = StateT (Trans.StateT s m a)
    deriving ( Generic
             , Functor, Applicative, Alternative, Monad, MonadPlus
             , MonadFix, MonadTrans, MonadIO, MFunctor )

-- | Type-restricted `coerce`.
pack :: Trans.StateT s m a -> StateT tag s m a
pack = coerce

-- | Type-restricted `coerce`.
unpack :: StateT tag s m a -> Trans.StateT s m a
unpack = coerce

instance MB.MonadBase b m => MB.MonadBase b (StateT tag s m) where
    liftBase = MB.liftBaseDefault

instance MC.MonadTransControl (StateT tag s) where
    type StT (StateT tag s) a = MC.StT (Trans.StateT s) a
    liftWith = MC.defaultLiftWith pack unpack
    restoreT = MC.defaultRestoreT pack

instance MC.MonadBaseControl b m => MC.MonadBaseControl b (StateT tag s m) where
    type StM (StateT tag s m) a = MC.ComposeSt (StateT tag s) m a
    liftBaseWith = MC.defaultLiftBaseWith
    restoreM = MC.defaultRestoreM

type instance Lift.StT (StateT tag s) a = MC.StT (StateT tag s) a

instance Lift.LiftLocal (StateT tag s) where
    liftLocal = Lift.defaultLiftLocal pack unpack

instance Lift.LiftCatch (StateT tag s) where
    liftCatch = Lift.defaultLiftCatch pack unpack

instance Lift.LiftListen (StateT tag s) where
    liftListen = Lift.defaultLiftListen pack unpack

instance Lift.LiftPass (StateT tag s) where
    liftPass = Lift.defaultLiftPass pack unpack

instance Lift.LiftCallCC (StateT tag s) where
    liftCallCC  = Lift.defaultLiftCallCC  pack unpack
    liftCallCC' = Lift.defaultLiftCallCC' pack unpack

tagged :: proxy tag -> Trans.StateT s m a -> StateT tag s m a
tagged _ = pack

untagged :: proxy tag -> StateT tag s m a -> Trans.StateT s m a
untagged _ = unpack

-- | Constructor for computations in the state monad transformer.
stateT :: proxy tag -> (s -> m (a, s)) -> StateT tag s m a
stateT t = tagged t . Trans.StateT

-- | Constructor for computations in the state monad
-- (the inverse of 'runState').
state :: Monad m => proxy tag -> (s -> (a, s)) -> StateT tag s m a
state t = tagged t . Trans.state

-- | Runs a 'StateT' with the given initial state
-- and returns both the final value and the final state.
runStateT :: proxy tag -> StateT tag s m a -> s -> m (a, s)
runStateT t = Trans.runStateT . untagged t

-- | Runs a 'StateT' with the given initial state
-- and returns the final value, discarding the final state.
evalStateT :: Monad m => proxy tag -> StateT tag s m a -> s -> m a
evalStateT t = Trans.evalStateT . untagged t

-- | Runs a 'StateT' with the given initial state
-- and returns the final state, discarding the final value.
execStateT :: Monad m => proxy tag -> StateT tag s m a -> s -> m s
execStateT t = Trans.execStateT . untagged t

-- | Runs a 'State' with the given initial state
-- and returns both the final value and the final state.
runState :: proxy tag -> State tag s a -> s -> (a, s)
runState t = Trans.runState . untagged t

-- | Runs a 'State' with the given initial state
-- and returns the final value, discarding the final state.
evalState :: proxy tag -> State tag s a -> s -> a
evalState t = Trans.evalState . untagged t

-- | Runs a 'State' with the given initial state
-- and returns the final state, discarding the final value.
execState :: proxy tag -> State tag s a -> s -> s
execState t = Trans.execState . untagged t

-- | Fetch the current value of the state within the monad.
get :: Monad m => proxy tag -> StateT tag s m s
get t = tagged t Trans.get

-- | Set the value of the state within the monad.
put :: Monad m => proxy tag -> s -> StateT tag s m ()
put t = tagged t . Trans.put

-- Instances for mtl classes

instance Class.MonadCont m => Class.MonadCont (StateT tag s m) where
    callCC = Lift.liftCallCC' Class.callCC

instance Class.MonadReader r m => Class.MonadReader r (StateT tag s m) where
    ask = lift Class.ask
    local = Lift.liftLocal Class.ask Class.local
    reader = lift . Class.reader

instance Class.MonadState s' m => Class.MonadState s' (StateT tag s m) where
    get = lift Class.get
    put = lift . Class.put
    state = lift . Class.state

instance Class.MonadWriter w m => Class.MonadWriter w (StateT tag s m) where
    writer = lift . Class.writer
    tell   = lift . Class.tell
    listen = Lift.liftListen Class.listen
    pass   = Lift.liftPass Class.pass

instance Class.MonadError e m => Class.MonadError e (StateT tag s m) where
    throwError = lift . Class.throwError
    catchError = Lift.liftCatch Class.catchError
