{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | See "Control.Monad.Writer.Class".

module Control.Monad.Ether.Writer.Class
    ( MonadWriter(..)
    , listens
    , censor
    ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Control.Monad.Trans.Ether.Writer hiding (writer, tell, listen, pass)
import qualified Control.Monad.Trans.Ether.Writer as W
import qualified Control.Monad.Trans.Lift.Listen as Lift
import qualified Control.Monad.Trans.Lift.Pass   as Lift

-- | See 'Control.Monad.Writer.MonadWriter'.
class (Monoid w, Monad m) => MonadWriter tag w m | m tag -> w where

    {-# MINIMAL (writer | tell), listen, pass #-}

    -- | Embed a simple writer action.
    writer :: proxy tag -> (a, w) -> m a
    writer t ~(a, w) = do
      tell t w
      return a

    -- | Append a value to the accumulator within the monad.
    tell :: proxy tag -> w -> m ()
    tell t w = writer t ((),w)

    -- | Execute an action and add its accumulator
    -- to the value of the computation.
    listen :: proxy tag -> m a -> m (a, w)

    -- | Execute an action which returns a value and a function,
    -- and return the value, applying the function to the accumulator.
    pass :: proxy tag -> m (a, w -> w) -> m a

-- | Execute an action and add the result of applying the given function to
-- its accumulator to the value of the computation.
listens :: MonadWriter tag w m => proxy tag -> (w -> b) -> m a -> m (a, b)
listens t f m = do
    ~(a, w) <- listen t m
    return (a, f w)

-- | Execute an action and apply a function to its accumulator.
censor :: MonadWriter tag w m => proxy tag -> (w -> w) -> m a -> m a
censor t f m = pass t $ do
    a <- m
    return (a, f)

instance {-# OVERLAPPING #-} (Monoid w, Monad m) => MonadWriter tag w (WriterT tag w m) where
    writer = W.writer
    tell = W.tell
    listen = W.listen
    pass = W.pass

instance ( Lift.LiftListen t
         , Lift.LiftPass   t
         , Monad (t m)
         , MonadWriter tag w m
         , Monoid w
         ) => MonadWriter tag w (t m) where
    writer t = Lift.lift . writer t
    tell   t = Lift.lift . tell t
    listen t = Lift.liftListen (listen t)
    pass   t = Lift.liftPass (pass t)
