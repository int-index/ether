{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | See "Control.Monad.State.Strict".

module Control.Monad.Ether.State.Strict
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
    -- *
    , STATE
    ) where

import Control.Monad.Ether.Handle
import Control.Monad.Ether.State.Common
import qualified Control.Monad.Trans.Ether.Handler as D
import qualified Control.Monad.Trans.State.Strict as T
import Data.Functor.Identity
import Data.Coerce

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
type StateT tag s = D.Handler (TAGGED STATE tag) (T.StateT s)

-- | Constructor for computations in the state monad transformer.
stateT :: forall tag s m a . (s -> m (a, s)) -> StateT tag s m a
stateT = coerce (T.StateT @s @m @a)
{-# INLINE stateT #-}

-- | Runs a 'StateT' with the given initial state
-- and returns both the final value and the final state.
runStateT :: forall tag s m a . StateT tag s m a -> s -> m (a, s)
runStateT = coerce (T.runStateT @s @m @a)
{-# INLINE runStateT #-}

-- | Runs a 'StateT' with the given initial state
-- and returns the final value, discarding the final state.
evalStateT :: forall tag s m a . Monad m => StateT tag s m a -> s -> m a
evalStateT = coerce (T.evalStateT @m @s @a)
{-# INLINE evalStateT #-}

-- | Runs a 'StateT' with the given initial state
-- and returns the final state, discarding the final value.
execStateT :: forall tag s m a . Monad m => StateT tag s m a -> s -> m s
execStateT = coerce (T.execStateT @m @s @a)
{-# INLINE execStateT #-}

-- | Runs a 'State' with the given initial state
-- and returns both the final value and the final state.
runState :: forall tag s a . State tag s a -> s -> (a, s)
runState = coerce (T.runState @s @a)
{-# INLINE runState #-}

-- | Runs a 'State' with the given initial state
-- and returns the final value, discarding the final state.
evalState :: forall tag s a . State tag s a -> s -> a
evalState = coerce (T.evalState @s @a)
{-# INLINE evalState #-}

-- | Runs a 'State' with the given initial state
-- and returns the final state, discarding the final value.
execState :: forall tag s a . State tag s a -> s -> s
execState = coerce (T.execState @s @a)
{-# INLINE execState #-}
