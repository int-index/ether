{-# LANGUAGE CPP #-}
module Control.Ether.Util
    ( fmap
    ) where

import Prelude hiding (fmap)

#if __GLASGOW_HASKELL__ < 710
import qualified Control.Monad
#else
import qualified Prelude
#endif

#if __GLASGOW_HASKELL__ < 710
fmap :: Monad f => (a -> b) -> f a -> f b
fmap = Control.Monad.liftM
#else
fmap :: Functor f => (a -> b) -> f a -> f b
fmap = Prelude.fmap
#endif

{-# INLINE fmap #-}
