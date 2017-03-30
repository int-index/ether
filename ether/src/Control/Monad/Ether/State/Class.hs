{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | See "Control.Monad.State.Class".

module Control.Monad.Ether.State.Class
  ( MonadState(..)
  ) where

import Control.Monad.Trans.Ether.Handler
import qualified Control.Monad.Trans as Lift
import Data.Coerce

-- | See 'Control.Monad.State.MonadState'.
class Monad m => MonadState tag s m | m tag -> s where

    {-# MINIMAL state | get, put #-}

    -- | Return the state from the internals of the monad.
    get :: m s
    get = state @tag (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: s -> m ()
    put s = state @tag (\_ -> ((), s))

    -- | Embed a simple state action into the monad.
    state :: (s -> (a, s)) -> m a
    state f = do
      s <- get @tag
      let ~(a, s') = f s
      put @tag s'
      return a

instance {-# OVERLAPPABLE #-}
    ( Lift.MonadTrans t
    , Monad (t m)
    , MonadState tag s m
    ) => MonadState tag s (t m)
  where
    get = Lift.lift (get @tag)
    put = Lift.lift . put @tag
    state = Lift.lift . state @tag

instance {-# OVERLAPPABLE #-}
    ( Monad (trans m)
    , MonadState tag s (Handler dps trans m)
    ) => MonadState tag s (Handler (dp ': dps) trans (m :: * -> *))
  where

    get =
      (coerce ::
        Handler        dps  trans m s ->
        Handler (dp ': dps) trans m s)
      (get @tag)
    {-# INLINE get #-}

    put =
      (coerce ::
        (s -> Handler        dps  trans m ()) ->
        (s -> Handler (dp ': dps) trans m ()))
      (put @tag)
    {-# INLINE put #-}

    state =
      (coerce :: forall a .
        ((s -> (a, s)) -> Handler        dps  trans m a) ->
        ((s -> (a, s)) -> Handler (dp ': dps) trans m a))
      (state @tag)
    {-# INLINE state #-}
