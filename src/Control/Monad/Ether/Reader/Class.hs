-- | See "Control.Monad.Reader.Class".

module Control.Monad.Ether.Reader.Class
  ( MonadReader(..)
  ) where

import Data.Kind as K
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
    {-# INLINE ask #-}

    local = Lift.liftLocal (ask @tag) (local @tag)
    {-# INLINE local #-}

    reader = Lift.lift . reader @tag
    {-# INLINE reader #-}

instance {-# OVERLAPPABLE #-}
    ( Monad (trans m)
    , MonadReader tag r (Handler effs trans m)
    ) => MonadReader tag r (Handler (eff ': effs) trans (m :: K.Type -> K.Type))
  where

    ask =
      (coerce ::
        Handler         effs  trans m r ->
        Handler (eff ': effs) trans m r)
      (ask @tag)
    {-# INLINE ask #-}

    local =
      (coerce :: forall a .
        Lift.Local r (Handler         effs  trans m) a ->
        Lift.Local r (Handler (eff ': effs) trans m) a)
      (local @tag)
    {-# INLINE local #-}

    reader =
      (coerce :: forall a .
        ((r -> a) -> Handler         effs  trans m a) ->
        ((r -> a) -> Handler (eff ': effs) trans m a))
      (reader @tag)
    {-# INLINE reader #-}
