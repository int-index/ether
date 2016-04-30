{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

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

import GHC.Prim (Proxy#, proxy#)
import qualified Control.Monad.Ether.State.Strict as Explicit

-- | See 'Control.Monad.Ether.State.Strict.StateT'.
type StateT s = Explicit.StateT s s

-- | See 'Control.Monad.Ether.State.Strict.State'.
type State  s = Explicit.State  s s

-- | See 'Control.Monad.Ether.State.Strict.stateT'.
stateT :: (s -> m (a, s)) -> StateT s m a
stateT = Explicit.stateT proxy#

-- | See 'Control.Monad.Ether.State.Strict.runStateT'.
runStateT :: StateT s m a -> s -> m (a, s)
runStateT = Explicit.runStateT proxy#

-- | See 'Control.Monad.Ether.State.Strict.runState'.
runState :: State s a -> s -> (a, s)
runState = Explicit.runState proxy#

-- | See 'Control.Monad.Ether.State.Strict.evalStateT'.
evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT = Explicit.evalStateT proxy#

-- | See 'Control.Monad.Ether.State.Strict.evalState'.
evalState :: State s a -> s -> a
evalState = Explicit.evalState proxy#

-- | See 'Control.Monad.Ether.State.Strict.execStateT'.
execStateT :: Monad m => StateT s m a -> s -> m s
execStateT = Explicit.execStateT proxy#

-- | See 'Control.Monad.Ether.State.Strict.execState'.
execState :: State s a -> s -> s
execState = Explicit.execState proxy#

-- | See 'Control.Monad.Ether.State.Strict.MonadState'.
type MonadState s = Explicit.MonadState s s

-- | See 'Control.Monad.Ether.State.Strict.get'.
get :: forall s m . MonadState s m => m s
get = Explicit.get (proxy# :: Proxy# s)

-- | See 'Control.Monad.Ether.State.Strict.gets'.
gets :: forall s m a . MonadState s m => (s -> a) -> m a
gets = Explicit.gets (proxy# :: Proxy# s)

-- | See 'Control.Monad.Ether.State.Strict.put'.
put :: forall s m . MonadState s m => s -> m ()
put = Explicit.put (proxy# :: Proxy# s)

-- | See 'Control.Monad.Ether.State.Strict.state'.
state :: forall s m a . MonadState s m => (s -> (a, s)) -> m a
state = Explicit.state (proxy# :: Proxy# s)

-- | See 'Control.Monad.Ether.State.Strict.modify'.
modify :: forall s m . MonadState s m => (s -> s) -> m ()
modify = Explicit.modify (proxy# :: Proxy# s)
