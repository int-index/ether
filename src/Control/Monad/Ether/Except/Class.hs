-- | See "Control.Monad.Except".

module Control.Monad.Ether.Except.Class
  ( MonadExcept(..)
  ) where

import qualified Control.Monad.Trans.Lift.Catch as Lift

-- | See 'Control.Monad.Except.MonadError'.
class Monad m => MonadExcept tag e m | m tag -> e where

    -- | Is used within a monadic computation to begin exception processing.
    throw :: e -> m a

    -- | A handler function to handle previous exceptions and return to
    -- normal execution.
    catch :: m a -> (e -> m a) -> m a

instance {-# OVERLAPPABLE #-}
         ( Lift.LiftCatch t
         , Monad (t m)
         , MonadExcept tag e m
         ) => MonadExcept tag e (t m) where

    throw = Lift.lift . throw @tag
    {-# INLINE throw #-}

    catch = Lift.liftCatch (catch @tag)
    {-# INLINE catch #-}
