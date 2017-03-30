{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | See "Control.Monad.Reader.Class".

module Control.Monad.Ether.Reader.Class
  ( MonadReader(..)
  ) where

import Control.Monad.Trans.Ether.Handler
import qualified Control.Monad.Trans.Lift.Local as Lift
import Data.Coerce

-- | See 'Control.Monad.Reader.MonadReader'.
class Monad m => MonadReader tag r m | m tag -> r where

    {-# MINIMAL (ask | reader), local #-}

    -- | Retrieves the monad environment.
    ask :: m r
    ask = reader @tag id

    -- | Executes a computation in a modified environment.
    local
        :: (r -> r)
        -- ^ The function to modify the environment.
        -> m a
        -- ^ @Reader@ to run in the modified environment.
        -> m a

    -- | Retrieves a function of the current environment.
    reader
        :: (r -> a)
        -- ^ The selector function to apply to the environment.
        -> m a
    reader f = fmap f (ask @tag)

instance {-# OVERLAPPABLE #-}
    ( Lift.LiftLocal t
    , Monad (t m)
    , MonadReader tag r m
    ) => MonadReader tag r (t m)
  where
    ask = Lift.lift (ask @tag)
    local = Lift.liftLocal (ask @tag) (local @tag)

instance {-# OVERLAPPABLE #-}
    ( Monad (trans m)
    , MonadReader tag r (Handler dps trans m)
    ) => MonadReader tag r (Handler (dp ': dps) trans (m :: * -> *))
  where

    ask =
      (coerce ::
        Handler        dps  trans m r ->
        Handler (dp ': dps) trans m r)
      (ask @tag)
    {-# INLINE ask #-}

    local =
      (coerce :: forall a .
        Lift.Local r (Handler dps trans m) a ->
        Lift.Local r (Handler (dp ': dps) trans m) a)
      (local @tag)
    {-# INLINE local #-}
