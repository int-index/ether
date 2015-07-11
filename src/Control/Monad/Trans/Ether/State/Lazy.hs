{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | See "Control.Monad.Trans.State.Lazy".

module Control.Monad.Trans.Ether.State.Lazy
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
    ) where

import Data.Functor.Identity (Identity(..))
import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (MFunctor)
import Control.Ether.Tagged (Taggable(..), Tagged(..))
import GHC.Generics (Generic)
import qualified Control.Newtype as NT

import qualified Control.Monad.Trans.Control as MC
import qualified Control.Monad.Trans.State.Lazy as Trans

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

instance NT.Newtype (StateT tag s m a)

instance MC.MonadTransControl (StateT tag s) where
    type StT (StateT tag s) a = MC.StT (Trans.StateT s) a
    liftWith = MC.defaultLiftWith NT.pack NT.unpack
    restoreT = MC.defaultRestoreT NT.pack

type instance Lift.StT (StateT tag s) a = MC.StT (StateT tag s) a

instance Lift.LiftLocal (StateT tag s) where
    liftLocal = Lift.defaultLiftLocal NT.pack NT.unpack

instance Lift.LiftCatch (StateT tag s) where
    liftCatch = Lift.defaultLiftCatch NT.pack NT.unpack

instance Lift.LiftListen (StateT tag s) where
    liftListen = Lift.defaultLiftListen NT.pack NT.unpack

instance Lift.LiftPass (StateT tag s) where
    liftPass = Lift.defaultLiftPass NT.pack NT.unpack

instance Lift.LiftCallCC (StateT tag s) where
    liftCallCC  = Lift.defaultLiftCallCC  NT.pack NT.unpack
    liftCallCC' = Lift.defaultLiftCallCC' NT.pack NT.unpack

instance Taggable (StateT tag s m) where
    type Tag (StateT tag s m) = 'Just tag
    type Inner (StateT tag s m) = 'Just m

instance Tagged (StateT tag s m) tag where
    type Untagged (StateT tag s m) = Trans.StateT s m

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
