{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}

module Control.Monad.Ether.State.Common
  ( MonadState
  , get
  , put
  , state
  , modify
  , gets
  ) where

import GHC.Prim (Proxy#, proxy#)
import Control.Monad.Ether.State.Class (MonadState)
import qualified Control.Monad.Ether.State.Class as C

-- | Return the state from the internals of the monad.
get :: forall tag s m . MonadState tag s m => m s
get = C.get (proxy# :: Proxy# tag)

-- | Replace the state inside the monad.
put :: forall tag s m . MonadState tag s m => s -> m ()
put = C.put (proxy# :: Proxy# tag)

-- | Embed a simple state action into the monad.
state :: forall tag s m a . MonadState tag s m => (s -> (a, s)) -> m a
state = C.state (proxy# :: Proxy# tag)

-- | Modifies the state inside a state monad.
modify :: forall tag s m . MonadState tag s m => (s -> s) -> m ()
modify f = state @tag (\s -> ((), f s))

-- | Gets specific component of the state, using a projection function supplied.
gets :: forall tag s m a . MonadState tag s m => (s -> a) -> m a
gets f = fmap f (get @tag)
