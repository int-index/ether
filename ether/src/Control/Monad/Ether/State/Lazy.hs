{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | See "Control.Monad.State.Lazy".

module Control.Monad.Ether.State.Lazy
    (
    -- * MonadState class
      MonadState
    , get
    , put
    , state
    , modify
    , gets
    -- * The State monad
    , State
    , runState
    , evalState
    , execState
    -- * The StateT monad transformer
    , StateT
    , stateT
    , runStateT
    , evalStateT
    , execStateT
    ) where

import Control.Monad.Ether.State.Common
import qualified Control.Monad.Ether.State.Class as C
import qualified Control.Monad.Trans.Ether.Dispatch as D
import qualified Control.Monad.Trans.State.Lazy as T
import Data.Functor.Identity

-- | Encode type-level information for 'StateT'.
data K_STATE t s = STATE t s

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
type StateT tag s = D.Dispatch (STATE tag s) (T.StateT s)

-- | Constructor for computations in the state monad transformer.
stateT :: forall tag s m a . (s -> m (a, s)) -> StateT tag s m a
stateT = D.pack . T.StateT

-- | Runs a 'StateT' with the given initial state
-- and returns both the final value and the final state.
runStateT :: forall tag s m a . StateT tag s m a -> s -> m (a, s)
runStateT = T.runStateT . D.unpack

-- | Runs a 'StateT' with the given initial state
-- and returns the final value, discarding the final state.
evalStateT :: forall tag s m a . Monad m => StateT tag s m a -> s -> m a
evalStateT = T.evalStateT . D.unpack

-- | Runs a 'StateT' with the given initial state
-- and returns the final state, discarding the final value.
execStateT :: forall tag s m a . Monad m => StateT tag s m a -> s -> m s
execStateT = T.execStateT . D.unpack

-- | Runs a 'State' with the given initial state
-- and returns both the final value and the final state.
runState :: forall tag s a . State tag s a -> s -> (a, s)
runState = T.runState . D.unpack

-- | Runs a 'State' with the given initial state
-- and returns the final value, discarding the final state.
evalState :: forall tag s a . State tag s a -> s -> a
evalState = T.evalState . D.unpack

-- | Runs a 'State' with the given initial state
-- and returns the final state, discarding the final value.
execState :: forall tag s a . State tag s a -> s -> s
execState = T.execState . D.unpack

instance
    ( Monad m, s ~ s', trans ~ T.StateT s
    ) => MonadState tag s (D.Dispatch (STATE tag s') trans m)
  where
    get _ = D.pack T.get
    put _ = D.pack . T.put
    state _ = D.pack . T.state
