{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | See "Control.Monad.Ether.State.Lazy".

module Control.Monad.Ether.Implicit.State.Lazy
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

import qualified Control.Monad.Ether.State.Lazy as A

-- | See 'Control.Monad.Ether.State.Lazy.StateT'.
type StateT s = A.StateT s s

-- | See 'Control.Monad.Ether.State.Lazy.stateT'.
stateT :: (s -> m (a, s)) -> StateT s m a
stateT = A.stateT

-- | See 'Control.Monad.Ether.State.Lazy.runStateT'.
runStateT :: StateT s m a -> s -> m (a, s)
runStateT = A.runStateT

-- | See 'Control.Monad.Ether.State.Lazy.runState'.
runState :: State s a -> s -> (a, s)
runState = A.runState

-- | See 'Control.Monad.Ether.State.Lazy.evalStateT'.
evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT = A.evalStateT

-- | See 'Control.Monad.Ether.State.Lazy.State'.
type State s = A.State s s

-- | See 'Control.Monad.Ether.State.Lazy.evalState'.
evalState :: State s a -> s -> a
evalState = A.evalState

-- | See 'Control.Monad.Ether.State.Lazy.execStateT'.
execStateT :: Monad m => StateT s m a -> s -> m s
execStateT = A.execStateT

-- | See 'Control.Monad.Ether.State.Lazy.execState'.
execState :: State s a -> s -> s
execState = A.execState

-- | See 'Control.Monad.Ether.State.Lazy.MonadState'.
type MonadState s = A.MonadState s s

-- | See 'Control.Monad.Ether.State.Lazy.get'.
get :: forall s m . MonadState s m => m s
get = A.get @s

-- | See 'Control.Monad.Ether.State.Lazy.gets'.
gets :: forall s m a . MonadState s m => (s -> a) -> m a
gets = A.gets @s

-- | See 'Control.Monad.Ether.State.Lazy.put'.
put :: forall s m . MonadState s m => s -> m ()
put = A.put @s

-- | See 'Control.Monad.Ether.State.Lazy.state'.
state :: forall s m a . MonadState s m => (s -> (a, s)) -> m a
state = A.state @s

-- | See 'Control.Monad.Ether.State.Lazy.modify'.
modify :: forall s m . MonadState s m => (s -> s) -> m ()
modify = A.modify @s
