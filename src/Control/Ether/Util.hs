{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Ether.Util
    ( type (++)
    , MaybeToList
    , fmap
    ) where

import Prelude hiding (fmap)

#if __GLASGOW_HASKELL__ < 710
import qualified Control.Monad
#else
import qualified Prelude
#endif

type family (as :: [*]) ++ (bs :: [*]) :: [*] where
    '[] ++ bs = bs
    (a ': as) ++ bs = a ': (as ++ bs)

type family MaybeToList (mt :: Maybe k) :: [k] where
    MaybeToList 'Nothing = '[]
    MaybeToList ('Just t) = '[t]

#if __GLASGOW_HASKELL__ < 710
fmap :: Monad f => (a -> b) -> f a -> f b
fmap = Control.Monad.liftM
#else
fmap :: Functor f => (a -> b) -> f a -> f b
fmap = Prelude.fmap
#endif

{-# INLINE fmap #-}
