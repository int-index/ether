module Control.Monad.Ether.State.Strict
    (
    -- * MonadState class
      MonadState(..)
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

import Control.Monad.Ether.State.Class
import Control.Monad.Trans.Ether.State.Strict hiding (state, get, put)
