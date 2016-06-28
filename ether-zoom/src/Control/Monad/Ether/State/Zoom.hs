{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Ether.State.Zoom
  ( TAG_ZOOM
  , TagZoomT
  , tagZoom
  ) where

import Control.Lens
import Data.Reflection
import Control.Monad.Trans.Ether.Dispatch
import Control.Monad.Trans.Identity
import Control.Monad.Ether.State.Class
import Data.Proxy

-- | Encode type-level information for 'tagZoom'.
data TAG_ZOOM t z

type TagZoomT t (z :: *) = Dispatch (TAG_ZOOM t z) IdentityT

-- | Zoom into a part of a state using a lens.
tagZoom
  :: forall tag sOuter sInner m a
   . Lens' sOuter sInner
  -> (forall z . Reifies z (ReifiedLens' sOuter sInner) => TagZoomT tag z m a)
  -> m a
tagZoom l m = reify (Lens l) (\(_ :: Proxy z) -> (runIdentityT . unpack) (m @z))

instance
    ( MonadState tag sOuter m
    , Reifies z (ReifiedLens' sOuter sInner)
    , trans ~ IdentityT
    ) => MonadState tag sInner (Dispatch (TAG_ZOOM tag z) trans m)
  where
    state t =
      let Lens l = reflect (Proxy :: Proxy z)
      in pack . IdentityT . state t . l
