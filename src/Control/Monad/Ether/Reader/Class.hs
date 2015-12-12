{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}

-- | See "Control.Monad.Reader.Class".

module Control.Monad.Ether.Reader.Class
    ( MonadReader(..)
    , asks
    ) where

import qualified Control.Monad.Trans.Ether.Reader as R
import qualified Control.Monad.Trans.Lift.Local as Lift
import qualified Control.Ether.Util as Util

-- | See 'Control.Monad.Reader.MonadReader'.
class Monad m => MonadReader tag r m | m tag -> r where

    {-# MINIMAL (ask | reader), local #-}

    -- | Retrieves the monad environment.
    ask :: proxy tag -> m r
    ask t = reader t id

    -- | Executes a computation in a modified environment.
    local
        :: proxy tag
        -> (r -> r)
        -- ^ The function to modify the environment.
        -> m a
        -- ^ @Reader@ to run in the modified environment.
        -> m a

    -- | Retrieves a function of the current environment.
    reader
        :: proxy tag
        -> (r -> a)
        -- ^ The selector function to apply to the environment.
        -> m a
    reader t f = Util.fmap f (ask t)

-- | Retrieves a function of the current environment.
asks
    :: MonadReader tag r m
    => proxy tag
    -> (r -> a)
    -- ^ The selector function to apply to the environment.
    -> m a
asks = reader

instance Monad m => MonadReader tag r (R.ReaderT tag r m) where
    ask = R.ask
    local = R.local
    reader = R.reader

instance {-# OVERLAPPABLE #-}
         ( Lift.LiftLocal t
         , Monad (t m)
         , MonadReader tag r m
         ) => MonadReader tag r (t m) where
    ask t = Lift.lift (ask t)
    local t = Lift.liftLocal (ask t) (local t)
