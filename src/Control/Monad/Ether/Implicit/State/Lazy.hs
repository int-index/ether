{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

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

import GHC.Prim (Proxy#, proxy#)
import qualified Control.Monad.Ether.State.Lazy as Explicit

-- | See 'Control.Monad.Ether.State.Lazy.StateT'.
type StateT s = Explicit.StateT s s

-- | See 'Control.Monad.Ether.State.Lazy.State'.
type State  s = Explicit.State  s s

-- | See 'Control.Monad.Ether.State.Lazy.stateT'.
stateT :: (s -> m (a, s)) -> StateT s m a
stateT = Explicit.stateT proxy#

-- | See 'Control.Monad.Ether.State.Lazy.runStateT'.
runStateT :: StateT s m a -> s -> m (a, s)
runStateT = Explicit.runStateT proxy#

-- | See 'Control.Monad.Ether.State.Lazy.runState'.
runState :: State s a -> s -> (a, s)
runState = Explicit.runState proxy#

-- | See 'Control.Monad.Ether.State.Lazy.evalStateT'.
evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT = Explicit.evalStateT proxy#

-- | See 'Control.Monad.Ether.State.Lazy.evalState'.
evalState :: State s a -> s -> a
evalState = Explicit.evalState proxy#

-- | See 'Control.Monad.Ether.State.Lazy.execStateT'.
execStateT :: Monad m => StateT s m a -> s -> m s
execStateT = Explicit.execStateT proxy#

-- | See 'Control.Monad.Ether.State.Lazy.execState'.
execState :: State s a -> s -> s
execState = Explicit.execState proxy#

-- | See 'Control.Monad.Ether.State.Lazy.MonadState'.
type MonadState s = Explicit.MonadState s s

-- | See 'Control.Monad.Ether.State.Lazy.get'.
get :: forall s m . MonadState s m => m s
get = Explicit.get (proxy# :: Proxy# s)

-- | See 'Control.Monad.Ether.State.Lazy.gets'.
gets :: forall s m a . MonadState s m => (s -> a) -> m a
gets = Explicit.gets (proxy# :: Proxy# s)

-- | See 'Control.Monad.Ether.State.Lazy.put'.
put :: forall s m . MonadState s m => s -> m ()
put = Explicit.put (proxy# :: Proxy# s)

-- | See 'Control.Monad.Ether.State.Lazy.state'.
state :: forall s m a . MonadState s m => (s -> (a, s)) -> m a
state = Explicit.state (proxy# :: Proxy# s)

-- | See 'Control.Monad.Ether.State.Lazy.modify'.
modify :: forall s m . MonadState s m => (s -> s) -> m ()
modify = Explicit.modify (proxy# :: Proxy# s)
