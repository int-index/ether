module Ether.Internal.HasLens
  ( LensLike
  , Lens
  , Lens'
  , HasLens(..)
  ) where

import Data.Tagged
import Data.Coerce

type LensLike f s t a b = (a -> f b) -> s -> f t

type Lens s t a b = forall f. Functor f => LensLike f s t a b

type Lens' s a = Lens s s a a

class HasLens tag outer inner | tag outer -> inner where
  lensOf :: Lens' outer inner

instance HasLens a a a where
  lensOf = id

instance HasLens t (Tagged t a) a where
  lensOf = \f -> fmap coerce . f . coerce
