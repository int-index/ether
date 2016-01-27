{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
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
import qualified Control.Monad.Trans.State.Lazy as Trans
import Control.Ether.TT

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
type StateT tag s = TT tag (Trans.StateT s)

-- | Constructor for computations in the state monad transformer.
stateT :: proxy tag -> (s -> m (a, s)) -> StateT tag s m a
stateT _ = pack . Trans.StateT

-- | Constructor for computations in the state monad
-- (the inverse of 'runState').
state :: Monad m => proxy tag -> (s -> (a, s)) -> StateT tag s m a
state _ = pack . Trans.state

-- | Runs a 'StateT' with the given initial state
-- and returns both the final value and the final state.
runStateT :: proxy tag -> StateT tag s m a -> s -> m (a, s)
runStateT _ = Trans.runStateT . unpack

-- | Runs a 'StateT' with the given initial state
-- and returns the final value, discarding the final state.
evalStateT :: Monad m => proxy tag -> StateT tag s m a -> s -> m a
evalStateT _ = Trans.evalStateT . unpack

-- | Runs a 'StateT' with the given initial state
-- and returns the final state, discarding the final value.
execStateT :: Monad m => proxy tag -> StateT tag s m a -> s -> m s
execStateT _ = Trans.execStateT . unpack

-- | Runs a 'State' with the given initial state
-- and returns both the final value and the final state.
runState :: proxy tag -> State tag s a -> s -> (a, s)
runState _ = Trans.runState . unpack

-- | Runs a 'State' with the given initial state
-- and returns the final value, discarding the final state.
evalState :: proxy tag -> State tag s a -> s -> a
evalState _ = Trans.evalState . unpack

-- | Runs a 'State' with the given initial state
-- and returns the final state, discarding the final value.
execState :: proxy tag -> State tag s a -> s -> s
execState _ = Trans.execState . unpack

-- | Fetch the current value of the state within the monad.
get :: Monad m => proxy tag -> StateT tag s m s
get _ = pack Trans.get

-- | Set the value of the state within the monad.
put :: Monad m => proxy tag -> s -> StateT tag s m ()
put _ = pack . Trans.put
