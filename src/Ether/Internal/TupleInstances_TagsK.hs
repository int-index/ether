module Ether.Internal.TupleInstances_TagsK () where

import Data.Tagged

import Ether.Internal.Tags
import Ether.Internal.TH_TupleInstances (makeTupleInstancesTagsK)

type instance TagsK () = '[]
type instance TagsK (Tagged t a) = '[KindOf t]
type instance TagsK (Tagged t0 a, Tagged t1 b) = '[KindOf t0, KindOf t1]

makeTupleInstancesTagsK
