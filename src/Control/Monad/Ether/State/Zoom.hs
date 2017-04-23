{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Control.Monad.Ether.State.Zoom
  ( TAG_ZOOM
  , TagZoomT
  , tagZoom
  ) where

import Control.Ether.Optic
import Data.Kind as K
import Data.Reflection
import Control.Monad.Trans.Ether.Handler
import Control.Monad.Trans.Identity
import Control.Monad.Ether.State.Class
import Data.Proxy
import Data.Coerce

-- | Encode type-level information for 'tagZoom'.
data TAG_ZOOM t z

type TagZoomT t (z :: K.Type) = Handler (TAG_ZOOM t z) IdentityT

newtype ReifiedLens s t a b = Lens (Lens s t a b)

type ReifiedLens' s a = ReifiedLens s s a a

-- | Zoom into a part of a state using a lens.
tagZoom
  :: forall tag sOuter sInner m a
   . Lens' sOuter sInner
  -> (forall z . Reifies z (ReifiedLens' sOuter sInner) => TagZoomT tag z m a)
  -> m a
tagZoom l m = reify (Lens l) (\(_ :: Proxy z) -> coerce (m @z))

instance
    ( MonadState tag sOuter m
    , Reifies z (ReifiedLens' sOuter sInner)
    , trans ~ IdentityT
    ) => MonadState tag sInner (Handler (TAG_ZOOM tag z) trans m)
  where
    state =
      (coerce :: forall dp r a .
        (r ->                  m a) ->
        (r -> Handler dp trans m a))
      (state @tag . l)
      where
        Lens l = reflect (Proxy :: Proxy z)
    {-# INLINE state #-}
