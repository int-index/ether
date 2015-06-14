
-- | See "Control.Monad.Except".

module Control.Monad.Ether.Except
    (
    -- * MonadExcept class
      MonadExcept(..)
    -- * The Except monad
    , Except
    , runExcept
    -- * The ExceptT monad transformer
    , ExceptT
    , exceptT
    , runExceptT
    , mapExceptT
    -- * Handle functions
    , handleT
    , handle
    ) where

import Control.Monad.Ether.Except.Class
import Control.Monad.Trans.Ether.Except hiding (throw, catch)
import Data.Functor.Identity (Identity(..))

-- | Runs an 'Except' and handles the exception with the given function.
handle :: proxy tag -> (e -> a) -> Except tag e a -> a
handle t h m = runIdentity (handleT t h m)

-- | Runs an 'ExceptT' and handles the exception with the given function.
handleT :: Functor m => proxy tag -> (e -> a) -> ExceptT tag e m a -> m a
handleT t h m = fmap (either h id) (runExceptT t m)
