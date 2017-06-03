{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Ether.Internal.TupleInstances () where

import Data.Tagged
import Ether.Internal.Tags
import Ether.Internal.TH_TupleInstances

type instance TagsK () = '[]
type instance TagsK (Tagged t a) = '[KindOf t]
type instance TagsK (Tagged t0 a, Tagged t1 b) = '[KindOf t0, KindOf t1]

makeTupleInstancesTagsK

type instance Tags () = 'HNil
type instance Tags (Tagged t a) = 'HCons t 'HNil
type instance Tags (Tagged t0 a, Tagged t1 b) = 'HCons t0 ('HCons t1 'HNil)

makeTupleInstancesTags

#ifndef DISABLE_TUP_INSTANCES
makeTupleInstancesHasLens [2..62]
#endif
