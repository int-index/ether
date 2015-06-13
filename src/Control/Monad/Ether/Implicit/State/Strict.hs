{-# LANGUAGE ConstraintKinds #-}
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
    , mapStateT
    ) where

import Data.Proxy
import qualified Control.Monad.Ether.State.Strict as Explicit

-- | See 'Control.Monad.Ether.State.Strict.StateT'.
type StateT s = Explicit.StateT s s

-- | See 'Control.Monad.Ether.State.Strict.State'.
type State  s = Explicit.State  s s

-- | See 'Control.Monad.Ether.State.Strict.stateT'.
stateT :: (s -> m (a, s)) -> StateT s m a
stateT = Explicit.stateT Proxy

-- | See 'Control.Monad.Ether.State.Strict.runStateT'.
runStateT :: StateT s m a -> s -> m (a, s)
runStateT = Explicit.runStateT Proxy

-- | See 'Control.Monad.Ether.State.Strict.runState'.
runState :: State s a -> s -> (a, s)
runState = Explicit.runState Proxy

-- | See 'Control.Monad.Ether.State.Strict.evalStateT'.
evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT = Explicit.evalStateT Proxy

-- | See 'Control.Monad.Ether.State.Strict.evalState'.
evalState :: State s a -> s -> a
evalState = Explicit.evalState Proxy

-- | See 'Control.Monad.Ether.State.Strict.execStateT'.
execStateT :: Monad m => StateT s m a -> s -> m s
execStateT = Explicit.execStateT Proxy

-- | See 'Control.Monad.Ether.State.Strict.execState'.
execState :: State s a -> s -> s
execState = Explicit.execState Proxy

-- | See 'Control.Monad.Ether.State.Strict.mapStateT'.
mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT = Explicit.mapStateT Proxy

-- | See 'Control.Monad.Ether.State.Strict.MonadState'.
type MonadState s = Explicit.MonadState s s

-- | See 'Control.Monad.Ether.State.Strict.get'.
get :: forall m s . MonadState s m => m s
get = Explicit.get (Proxy :: Proxy s)

-- | See 'Control.Monad.Ether.State.Strict.gets'.
gets :: forall m s a . MonadState s m => (s -> a) -> m a
gets = Explicit.gets (Proxy :: Proxy s)

-- | See 'Control.Monad.Ether.State.Strict.put'.
put :: forall m s . MonadState s m => s -> m ()
put = Explicit.put (Proxy :: Proxy s)

-- | See 'Control.Monad.Ether.State.Strict.state'.
state :: forall m s a . MonadState s m => (s -> (a, s)) -> m a
state = Explicit.state (Proxy :: Proxy s)

-- | See 'Control.Monad.Ether.State.Strict.modify'.
modify :: forall m s . MonadState s m => (s -> s) -> m ()
modify = Explicit.modify (Proxy :: Proxy s)
