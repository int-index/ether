module Ether.Internal.TupleInstances_Tags () where

import Data.Tagged

import Ether.Internal.Tags
import Ether.Internal.TupleInstances_TagsK ()
import Ether.Internal.TH_TupleInstances (makeTupleInstancesTags)

type instance Tags () = 'HNil
type instance Tags (Tagged t a) = 'HCons t 'HNil
type instance Tags (Tagged t0 a, Tagged t1 b) = 'HCons t0 ('HCons t1 'HNil)

makeTupleInstancesTags
