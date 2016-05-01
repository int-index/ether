
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
    ) where

import Control.Monad.Ether.State.Common
import Control.Monad.Trans.Ether.State.Strict hiding (state, get, put)
