{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}

-- | See "Control.Monad.State.Class".

module Control.Monad.Ether.State.Class
    ( MonadState(..)
    , modify
    , gets
    ) where

import GHC.Prim (Proxy#)
import qualified Control.Monad.Trans.Ether.State.Lazy   as S.L
import qualified Control.Monad.Trans.Ether.State.Strict as S.S
import qualified Control.Monad.Trans as Lift

-- | See 'Control.Monad.State.MonadState'.
class Monad m => MonadState tag s m | m tag -> s where

    {-# MINIMAL state | get, put #-}

    -- | Return the state from the internals of the monad.
    get :: Proxy# tag -> m s
    get t = state t (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: Proxy# tag -> s -> m ()
    put t s = state t (\_ -> ((), s))

    -- | Embed a simple state action into the monad.
    state :: Proxy# tag -> (s -> (a, s)) -> m a
    state t f = do
        s <- get t
        let ~(a, s') = f s
        put t s'
        return a

-- | Modifies the state inside a state monad.
modify :: MonadState tag s m => Proxy# tag -> (s -> s) -> m ()
modify t f = state t (\s -> ((), f s))

-- | Gets specific component of the state, using a projection function supplied.
gets :: MonadState tag s m => Proxy# tag -> (s -> a) -> m a
gets t f = fmap f (get t)

instance (Monad m, s ~ s') => MonadState tag s (S.L.StateT tag s' m) where
    get = S.L.get
    put = S.L.put
    state = S.L.state

instance (Monad m, s ~ s') => MonadState tag s (S.S.StateT tag s' m) where
    get = S.S.get
    put = S.S.put
    state = S.S.state

instance {-# OVERLAPPABLE #-}
         ( Lift.MonadTrans t
         , Monad (t m)
         , MonadState tag s m
         ) => MonadState tag s (t m) where
    get t = Lift.lift (get t)
    put t = Lift.lift . put t
    state t = Lift.lift . state t
