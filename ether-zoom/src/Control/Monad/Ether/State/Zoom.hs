{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Ether.State.Zoom
  ( K_TagZoom(..)
  , tagZoom
  ) where

import Control.Lens
import Data.Reflection
import Control.Monad.Trans.Ether.Dispatch
import Control.Monad.Ether.State.Class
import Data.Proxy

-- | Encode type-level information for 'tagZoom'.
data K_TagZoom t z = TagZoom t z

type DispatchTagZoomT t (z :: *) = DispatchT (TagZoom t z)

-- | Zoom into a part of a state using a lens.
tagZoom
  :: forall tag sOuter sInner m a
   . Lens' sOuter sInner
  -> (forall z . Reifies z (ReifiedLens' sOuter sInner) => DispatchTagZoomT tag z m a)
  -> m a
tagZoom l m = reify (Lens l) (\(_ :: Proxy z) -> runDispatchT (m @z))

instance ( MonadState tag sOuter m
         , Reifies z (ReifiedLens' sOuter sInner)
         ) => MonadState tag sInner (DispatchTagZoomT tag z m) where
  state t = dispatchT . state t . l
    where
      Lens l = reflect (Proxy :: Proxy z)

{- Example usage.

import Control.Monad.Ether.State
import Control.Monad.Ether.State.Zoom

data Foo

example :: Enum a => (a, a) -> (a, a)
example = evalState @Foo $ do
  tagZoom @Foo _1 $ modify @Foo succ
  tagZoom @Foo _2 $ modify @Foo pred
  get @Foo

-}
