module Ether.Internal.Tags
  ( Tags
  , TagsK
  , HList(..)
  , KindOf
  ) where

import Data.Kind

data HList xs where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

type KindOf (a :: k) = k

type family TagsK (p :: Type) :: [Type]
type family Tags  (p :: Type) :: HList (TagsK p)

