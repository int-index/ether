{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    , mapStateT
    ) where

import Data.Proxy
import qualified Control.Monad.Ether.State.Lazy as Explicit

type StateT s = Explicit.StateT s s
type State  s = Explicit.State  s s

stateT :: (s -> m (a, s)) -> StateT s m a
stateT = Explicit.stateT Proxy

runStateT :: StateT s m a -> s -> m (a, s)
runStateT = Explicit.runStateT Proxy

runState :: State s a -> s -> (a, s)
runState = Explicit.runState Proxy

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT = Explicit.evalStateT Proxy

evalState :: State s a -> s -> a
evalState = Explicit.evalState Proxy

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT = Explicit.execStateT Proxy

execState :: State s a -> s -> s
execState = Explicit.execState Proxy

mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT = Explicit.mapStateT Proxy

type MonadState s = Explicit.MonadState s s

get :: forall m s . MonadState s m => m s
get = Explicit.get (Proxy :: Proxy s)

gets :: forall m s a . MonadState s m => (s -> a) -> m a
gets = Explicit.gets (Proxy :: Proxy s)

put :: forall m s . MonadState s m => s -> m ()
put = Explicit.put (Proxy :: Proxy s)

state :: forall m s a . MonadState s m => (s -> (a, s)) -> m a
state = Explicit.state (Proxy :: Proxy s)

modify :: forall m s . MonadState s m => (s -> s) -> m ()
modify = Explicit.modify (Proxy :: Proxy s)
