{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Ether.State.Common
  ( MonadState
  , get
  , put
  , state
  , modify
  , gets
  , STATE
  ) where

import Control.Monad.Ether.State.Class
import qualified Control.Monad.State as T
import qualified Control.Monad.Trans.Ether.Handler as D
import Data.Coerce

-- | Encode type-level information for 'StateT'.
data STATE

-- | Modifies the state inside a state monad.
modify :: forall tag s m . MonadState tag s m => (s -> s) -> m ()
modify f = state @tag (\s -> ((), f s))
{-# INLINABLE modify #-}

-- | Gets specific component of the state, using a projection function supplied.
gets :: forall tag s m a . MonadState tag s m => (s -> a) -> m a
gets f = fmap f (get @tag)
{-# INLINABLE gets #-}

instance
    ( T.MonadState s (trans m) -- FIXME: (forall m . T.MonadState s (trans m))
    ) => MonadState tag s (D.Handler '(STATE, tag) trans (m :: * -> *))
  where

    get = coerce (T.get @s @(trans m))
    {-# INLINE get #-}

    put = coerce (T.put @s @(trans m))
    {-# INLINE put #-}

    state =
      coerce (T.state @s @(trans m) @a) ::
        forall dp a . (s -> (a, s)) -> D.Handler dp trans m a
    {-# INLINE state #-}
