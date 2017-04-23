module Control.Monad.Ether.State.Flatten
  ( StateT
  , runStateT
  , State
  , runState
  ) where

import Control.Ether.Optic
import Control.Monad.Ether.Handle
import Control.Monad.Ether.State (STATE)
import qualified Control.Monad.State as T
import Control.Monad.Trans.Ether.Handler
import Data.Coerce
import Data.Functor.Identity
import Data.Kind

type family STATES (ts :: HList xs) :: [Type] where
  STATES 'HNil = '[]
  STATES ('HCons t ts) = TAGGED STATE t ': STATES ts

type StateT s = Handler (STATES (Tags s)) (T.StateT s)

type State s = StateT s Identity

runStateT :: forall p m a . StateT p m a -> p -> m (a, p)
runStateT = coerce (T.runStateT @p @m @a)

runState :: forall p a . State p a -> p -> (a, p)
runState = coerce (T.runState @p @a)
