{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | See "Control.Monad.Except".

module Control.Monad.Ether.Except.Class
    ( MonadExcept(..)
    ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Control.Monad.Trans.Ether.Except hiding (throw, catch)
import qualified Control.Monad.Trans.Ether.Except as E
import qualified Control.Monad.Trans.Lift.Catch as Lift


-- | See 'Control.Monad.Except.MonadError'.
class Monad m => MonadExcept tag e m | m tag -> e where

    -- | Is used within a monadic computation to begin exception processing.
    throw :: proxy tag -> e -> m a

    -- | A handler function to handle previous exceptions and return to
    -- normal execution.
    catch :: proxy tag -> m a -> (e -> m a) -> m a

instance {-# OVERLAPPING #-} Monad m => MonadExcept tag e (ExceptT tag e m) where
    throw = E.throw
    catch = E.catch

instance ( Lift.LiftCatch t
         , Monad (t m)
         , MonadExcept tag e m
         ) => MonadExcept tag e (t m) where
    throw t = Lift.lift . throw t
    catch t = Lift.liftCatch (catch t)
