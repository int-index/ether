{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | See "Control.Monad.Writer.Class".

module Control.Monad.Ether.Writer.Class
  ( MonadWriter(..)
  ) where

import qualified Control.Monad.Trans.Lift.Listen as Lift
import qualified Control.Monad.Trans.Lift.Pass   as Lift

-- | See 'Control.Monad.Writer.MonadWriter'.
class (Monoid w, Monad m) => MonadWriter tag w m | m tag -> w where

    {-# MINIMAL (writer | tell), listen, pass #-}

    -- | Embed a simple writer action.
    writer :: (a, w) -> m a
    writer ~(a, w) = do
      tell @tag w
      return a

    -- | Append a value to the accumulator within the monad.
    tell :: w -> m ()
    tell w = writer @tag ((),w)

    -- | Execute an action and add its accumulator
    -- to the value of the computation.
    listen :: m a -> m (a, w)

    -- | Execute an action which returns a value and a function,
    -- and return the value, applying the function to the accumulator.
    pass ::  m (a, w -> w) -> m a

instance {-# OVERLAPPABLE #-}
         ( Lift.LiftListen t
         , Lift.LiftPass   t
         , Monad (t m)
         , MonadWriter tag w m
         , Monoid w
         ) => MonadWriter tag w (t m) where
    writer = Lift.lift . writer @tag
    tell   = Lift.lift . tell @tag
    listen = Lift.liftListen (listen @tag)
    pass   = Lift.liftPass (pass @tag)
