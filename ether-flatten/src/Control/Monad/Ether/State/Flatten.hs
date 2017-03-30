{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Ether.State.Flatten
  ( runState
  , runStateT
  ) where

import Data.Functor.Identity
import Control.Monad.Ether.State.Class as C
import Control.Monad.Trans.Ether.Handler
import qualified Control.Monad.State as T
import Control.Lens
import Control.Ether.Flatten
import Data.Coerce

data STATE

type family STATES ts where
  STATES '[] = '[]
  STATES (t ': ts) = '(STATE, t) ': STATES ts

type StateT ts s = Handler (STATES ts) (T.StateT s)

type State ts s = StateT ts s Identity

instance
    ( Monad m, HasLens tag payload s
    , T.MonadState payload (trans m) -- FIXME: (forall m . T.MonadState payload (trans m))
    ) => C.MonadState tag s (Handler ('(STATE, tag) ': dps) trans m)
  where

    get =
      (coerce :: forall dp a .
                   trans m a ->
        Handler dp trans m a)
      (use (lensOf @tag))
    {-# INLINE get #-}

    put s =
      (coerce :: forall dp a .
                   trans m a ->
        Handler dp trans m a)
      (T.modify (lensOf @tag .~ s))
    {-# INLINE put #-}

    state f =
      (coerce :: forall dp a .
                   trans m a ->
        Handler dp trans m a)
      (T.state (lensOf @tag f))
    {-# INLINE state #-}

runStateT
  :: StateT tags (Product tags as) m a
  -> Product tags as
  -> m (a, Product tags as)
runStateT m = T.runStateT (coerce m)

runState
  :: State tags (Product tags as) a
  -> Product tags as
  -> (a, Product tags as)
runState m = T.runState (coerce m)
