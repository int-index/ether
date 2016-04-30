{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

-- | See "Control.Monad.Ether.State.Lazy".

module Control.Monad.Ether.Ambiguous.State.Lazy
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
import Control.Monad.Ether.State.Lazy (MonadState, StateT, State)
import qualified Control.Monad.Ether.State.Lazy as Explicit

-- | See 'Control.Monad.Ether.State.Lazy.stateT'.
stateT :: forall tag s m a . (s -> m (a, s)) -> StateT tag s m a
stateT = Explicit.stateT proxy#

-- | See 'Control.Monad.Ether.State.Lazy.runStateT'.
runStateT :: forall tag s m a . StateT tag s m a -> s -> m (a, s)
runStateT = Explicit.runStateT proxy#

-- | See 'Control.Monad.Ether.State.Lazy.runState'.
runState :: forall tag s a . State tag s a -> s -> (a, s)
runState = Explicit.runState proxy#

-- | See 'Control.Monad.Ether.State.Lazy.evalStateT'.
evalStateT :: forall tag s m a . Monad m => StateT tag s m a -> s -> m a
evalStateT = Explicit.evalStateT proxy#

-- | See 'Control.Monad.Ether.State.Lazy.evalState'.
evalState :: forall tag s a . State tag s a -> s -> a
evalState = Explicit.evalState proxy#

-- | See 'Control.Monad.Ether.State.Lazy.execStateT'.
execStateT :: forall tag s m a . Monad m => StateT tag s m a -> s -> m s
execStateT = Explicit.execStateT proxy#

-- | See 'Control.Monad.Ether.State.Lazy.execState'.
execState :: forall tag s a . State tag s a -> s -> s
execState = Explicit.execState proxy#

-- | See 'Control.Monad.Ether.State.Lazy.get'.
get :: forall tag s m . MonadState tag s m => m s
get = Explicit.get (proxy# :: Proxy# tag)

-- | See 'Control.Monad.Ether.State.Lazy.gets'.
gets :: forall tag s m a . MonadState tag s m => (s -> a) -> m a
gets = Explicit.gets (proxy# :: Proxy# tag)

-- | See 'Control.Monad.Ether.State.Lazy.put'.
put :: forall tag s m . MonadState tag s m => s -> m ()
put = Explicit.put (proxy# :: Proxy# tag)

-- | See 'Control.Monad.Ether.State.Lazy.state'.
state :: forall tag s m a . MonadState tag s m => (s -> (a, s)) -> m a
state = Explicit.state (proxy# :: Proxy# tag)

-- | See 'Control.Monad.Ether.State.Lazy.modify'.
modify :: forall tag s m . MonadState tag s m => (s -> s) -> m ()
modify = Explicit.modify (proxy# :: Proxy# tag)
