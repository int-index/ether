module Control.Monad.Ether.State.Common
  ( MonadState
  , get
  , put
  , state
  , modify
  , gets
  , STATE
  ) where

import Control.Ether.Optic
import Control.Monad.Ether.Handle
import Control.Monad.Ether.State.Class
import qualified Control.Monad.State as T
import Control.Monad.Trans.Ether.Handler
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

type instance HandleSuper      STATE s trans   = ()
type instance HandleConstraint STATE s trans m =
  T.MonadState s (trans m)

instance Handle STATE s (T.StateT s) where
  handling r = r
  {-# INLINE handling #-}

instance
    ( Handle STATE s trans
    , Monad m, Monad (trans m)
    ) => MonadState tag s (Handler (TAGGED STATE tag) trans m)
  where

    get =
      handling @STATE @s @trans @m $
      coerce (T.get @s @(trans m))
    {-# INLINE get #-}

    put =
      handling @STATE @s @trans @m $
      coerce (T.put @s @(trans m))
    {-# INLINE put #-}

    state =
      handling @STATE @s @trans @m $
      coerce (T.state @s @(trans m) @a) ::
        forall eff a . (s -> (a, s)) -> Handler eff trans m a
    {-# INLINE state #-}

instance
    ( HasLens tag payload s
    , Handle STATE payload trans
    , Monad m, Monad (trans m)
    ) => MonadState tag s (Handler (TAGGED STATE tag ': effs) trans m)
  where

    get =
      handling @STATE @payload @trans @m $
      (coerce :: forall eff a .
                    trans m a ->
        Handler eff trans m a)
      (T.gets (view (lensOf @tag @payload @s)))
    {-# INLINE get #-}

    put s =
      handling @STATE @payload @trans @m $
      (coerce :: forall eff a .
                    trans m a ->
        Handler eff trans m a)
      (T.modify (over (lensOf @tag @payload @s) (const s)))
    {-# INLINE put #-}

    state f =
      handling @STATE @payload @trans @m $
      (coerce :: forall eff a .
                    trans m a ->
        Handler eff trans m a)
      (T.state (lensOf @tag @payload @s f))
    {-# INLINE state #-}
