
-- | See "Control.Monad.State.Lazy".

module Control.Monad.Ether.State.Lazy
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
    ) where

import Control.Monad.Ether.State.Class
import Control.Monad.Trans.Ether.State.Lazy hiding (state, get, put)
