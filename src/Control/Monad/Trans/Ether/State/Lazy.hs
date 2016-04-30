{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Control.Monad.Trans.State.Lazy as Trans
import Control.Monad.Trans.Ether.Tagged

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
type StateT tag s = TaggedTrans tag (Trans.StateT s)

-- | Constructor for computations in the state monad transformer.
stateT :: forall tag s m a . (s -> m (a, s)) -> StateT tag s m a
stateT = pack . Trans.StateT

-- | Constructor for computations in the state monad
-- (the inverse of 'runState').
state :: forall tag s m a . Monad m => (s -> (a, s)) -> StateT tag s m a
state = pack . Trans.state

-- | Runs a 'StateT' with the given initial state
-- and returns both the final value and the final state.
runStateT :: forall tag s m a . StateT tag s m a -> s -> m (a, s)
runStateT = Trans.runStateT . unpack

-- | Runs a 'StateT' with the given initial state
-- and returns the final value, discarding the final state.
evalStateT :: forall tag s m a . Monad m => StateT tag s m a -> s -> m a
evalStateT = Trans.evalStateT . unpack

-- | Runs a 'StateT' with the given initial state
-- and returns the final state, discarding the final value.
execStateT :: forall tag s m a . Monad m => StateT tag s m a -> s -> m s
execStateT = Trans.execStateT . unpack

-- | Runs a 'State' with the given initial state
-- and returns both the final value and the final state.
runState :: forall tag s a . State tag s a -> s -> (a, s)
runState = Trans.runState . unpack

-- | Runs a 'State' with the given initial state
-- and returns the final value, discarding the final state.
evalState :: forall tag s a . State tag s a -> s -> a
evalState = Trans.evalState . unpack

-- | Runs a 'State' with the given initial state
-- and returns the final state, discarding the final value.
execState :: forall tag s a . State tag s a -> s -> s
execState = Trans.execState . unpack

-- | Fetch the current value of the state within the monad.
get :: forall tag s m . Monad m => StateT tag s m s
get = pack Trans.get

-- | Set the value of the state within the monad.
put :: forall tag s m . Monad m => s -> StateT tag s m ()
put = pack . Trans.put
