{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | See "Control.Monad.Ether.State.Strict".

module Control.Monad.Ether.Implicit.State.Strict
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

import qualified Control.Monad.Ether.State.Strict as A

-- | See 'Control.Monad.Ether.State.Strict.StateT'.
type StateT s = A.StateT s s

-- | See 'Control.Monad.Ether.State.Strict.stateT'.
stateT :: (s -> m (a, s)) -> StateT s m a
stateT = A.stateT

-- | See 'Control.Monad.Ether.State.Strict.runStateT'.
runStateT :: StateT s m a -> s -> m (a, s)
runStateT = A.runStateT

-- | See 'Control.Monad.Ether.State.Strict.runState'.
runState :: State s a -> s -> (a, s)
runState = A.runState

-- | See 'Control.Monad.Ether.State.Strict.evalStateT'.
evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT = A.evalStateT

-- | See 'Control.Monad.Ether.State.Strict.State'.
type State s = A.State s s

-- | See 'Control.Monad.Ether.State.Strict.evalState'.
evalState :: State s a -> s -> a
evalState = A.evalState

-- | See 'Control.Monad.Ether.State.Strict.execStateT'.
execStateT :: Monad m => StateT s m a -> s -> m s
execStateT = A.execStateT

-- | See 'Control.Monad.Ether.State.Strict.execState'.
execState :: State s a -> s -> s
execState = A.execState

-- | See 'Control.Monad.Ether.State.Strict.MonadState'.
type MonadState s = A.MonadState s s

-- | See 'Control.Monad.Ether.State.Strict.get'.
get :: forall s m . MonadState s m => m s
get = A.get @s

-- | See 'Control.Monad.Ether.State.Strict.gets'.
gets :: forall s m a . MonadState s m => (s -> a) -> m a
gets = A.gets @s

-- | See 'Control.Monad.Ether.State.Strict.put'.
put :: forall s m . MonadState s m => s -> m ()
put = A.put @s

-- | See 'Control.Monad.Ether.State.Strict.state'.
state :: forall s m a . MonadState s m => (s -> (a, s)) -> m a
state = A.state @s

-- | See 'Control.Monad.Ether.State.Strict.modify'.
modify :: forall s m . MonadState s m => (s -> s) -> m ()
modify = A.modify @s
