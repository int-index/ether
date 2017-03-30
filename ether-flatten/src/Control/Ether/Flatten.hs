{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Ether.Flatten
  ( HasLens(..)
  , Product(..)
  ) where

import Control.DeepSeq
import Control.Lens
import GHC.Generics (Generic)

class HasLens tag outer inner | tag outer -> inner where
  lensOf :: Lens' outer inner

data family Product :: [k] -> [*] -> *

data instance Product ts '[a1, a2, a3, a4] =
  P4
    { p4_1 :: !a1
    , p4_2 :: !a2
    , p4_3 :: !a3
    , p4_4 :: !a4
    } deriving (Generic)

deriving instance
  ( Show a1
  , Show a2
  , Show a3
  , Show a4
  ) => Show (Product ts '[a1, a2, a3, a4])

instance
  ( NFData a1
  , NFData a2
  , NFData a3
  , NFData a4
  ) => NFData (Product ts '[a1, a2, a3, a4])

instance a ~ a1 => HasLens t1
  (Product
    '[t1, t2, t3, t4]
    '[a1, a2, a3, a4]) a where
  lensOf = lens p4_1 (\p a -> p{ p4_1 = a })
  {-# INLINE lensOf #-}

instance a ~ a2 => HasLens t2
  (Product
    '[t1, t2, t3, t4]
    '[a1, a2, a3, a4]) a where
  lensOf = lens p4_2 (\p a -> p{ p4_2 = a })
  {-# INLINE lensOf #-}

instance a ~ a3 => HasLens t3
  (Product
    '[t1, t2, t3, t4]
    '[a1, a2, a3, a4]) a where
  lensOf = lens p4_3 (\p a -> p{ p4_3 = a })
  {-# INLINE lensOf #-}

instance a ~ a4 => HasLens t4
  (Product
    '[t1, t2, t3, t4]
    '[a1, a2, a3, a4]) a where
  lensOf = lens p4_4 (\p a -> p{ p4_4 = a })
  {-# INLINE lensOf #-}

data instance Product ts '[a1, a2, a3, a4, a5] =
  P5
    { p5_1 :: !a1
    , p5_2 :: !a2
    , p5_3 :: !a3
    , p5_4 :: !a4
    , p5_5 :: !a5
    } deriving (Generic)

deriving instance
  ( Show a1
  , Show a2
  , Show a3
  , Show a4
  , Show a5
  ) => Show (Product ts '[a1, a2, a3, a4, a5])

instance
  ( NFData a1
  , NFData a2
  , NFData a3
  , NFData a4
  , NFData a5
  ) => NFData (Product ts '[a1, a2, a3, a4, a5])

instance a ~ a1 => HasLens t1
  (Product
    '[t1, t2, t3, t4, t5]
    '[a1, a2, a3, a4, a5]) a where
  lensOf = lens p5_1 (\p a -> p{ p5_1 = a })
  {-# INLINE lensOf #-}

instance a ~ a2 => HasLens t2
  (Product
    '[t1, t2, t3, t4, t5]
    '[a1, a2, a3, a4, a5]) a where
  lensOf = lens p5_2 (\p a -> p{ p5_2 = a })
  {-# INLINE lensOf #-}

instance a ~ a3 => HasLens t3
  (Product
    '[t1, t2, t3, t4, t5]
    '[a1, a2, a3, a4, a5]) a where
  lensOf = lens p5_3 (\p a -> p{ p5_3 = a })
  {-# INLINE lensOf #-}

instance a ~ a4 => HasLens t4
  (Product
    '[t1, t2, t3, t4, t5]
    '[a1, a2, a3, a4, a5]) a where
  lensOf = lens p5_4 (\p a -> p{ p5_4 = a })
  {-# INLINE lensOf #-}

instance a ~ a5 => HasLens t5
  (Product
    '[t1, t2, t3, t4, t5]
    '[a1, a2, a3, a4, a5]) a where
  lensOf = lens p5_5 (\p a -> p{ p5_5 = a })
  {-# INLINE lensOf #-}

data instance Product ts '[a1, a2, a3, a4, a5, a6, a7, a8, a9] =
  P9
    { p9_1 :: !a1
    , p9_2 :: !a2
    , p9_3 :: !a3
    , p9_4 :: !a4
    , p9_5 :: !a5
    , p9_6 :: !a6
    , p9_7 :: !a7
    , p9_8 :: !a8
    , p9_9 :: !a9
    } deriving (Generic)

deriving instance
  ( Show a1
  , Show a2
  , Show a3
  , Show a4
  , Show a5
  , Show a6
  , Show a7
  , Show a8
  , Show a9
  ) => Show (Product ts '[a1, a2, a3, a4, a5, a6, a7, a8, a9])

instance
  ( NFData a1
  , NFData a2
  , NFData a3
  , NFData a4
  , NFData a5
  , NFData a6
  , NFData a7
  , NFData a8
  , NFData a9
  ) => NFData (Product ts '[a1, a2, a3, a4, a5, a6, a7, a8, a9])

instance a ~ a1 => HasLens t1
  (Product
    '[t1, t2, t3, t4, t5, t6, t7, t8, t9]
    '[a1, a2, a3, a4, a5, a6, a7, a8, a9]) a where
  lensOf = lens p9_1 (\p a -> p{ p9_1 = a })
  {-# INLINE lensOf #-}

instance a ~ a2 => HasLens t2
  (Product
    '[t1, t2, t3, t4, t5, t6, t7, t8, t9]
    '[a1, a2, a3, a4, a5, a6, a7, a8, a9]) a where
  lensOf = lens p9_2 (\p a -> p{ p9_2 = a })
  {-# INLINE lensOf #-}

instance a ~ a3 => HasLens t3
  (Product
    '[t1, t2, t3, t4, t5, t6, t7, t8, t9]
    '[a1, a2, a3, a4, a5, a6, a7, a8, a9]) a where
  lensOf = lens p9_3 (\p a -> p{ p9_3 = a })
  {-# INLINE lensOf #-}

instance a ~ a4 => HasLens t4
  (Product
    '[t1, t2, t3, t4, t5, t6, t7, t8, t9]
    '[a1, a2, a3, a4, a5, a6, a7, a8, a9]) a where
  lensOf = lens p9_4 (\p a -> p{ p9_4 = a })
  {-# INLINE lensOf #-}

instance a ~ a5 => HasLens t5
  (Product
    '[t1, t2, t3, t4, t5, t6, t7, t8, t9]
    '[a1, a2, a3, a4, a5, a6, a7, a8, a9]) a where
  lensOf = lens p9_5 (\p a -> p{ p9_5 = a })
  {-# INLINE lensOf #-}

instance a ~ a6 => HasLens t6
  (Product
    '[t1, t2, t3, t4, t5, t6, t7, t8, t9]
    '[a1, a2, a3, a4, a5, a6, a7, a8, a9]) a where
  lensOf = lens p9_6 (\p a -> p{ p9_6 = a })
  {-# INLINE lensOf #-}

instance a ~ a7 => HasLens t7
  (Product
    '[t1, t2, t3, t4, t5, t6, t7, t8, t9]
    '[a1, a2, a3, a4, a5, a6, a7, a8, a9]) a where
  lensOf = lens p9_7 (\p a -> p{ p9_7 = a })
  {-# INLINE lensOf #-}

instance a ~ a8 => HasLens t8
  (Product
    '[t1, t2, t3, t4, t5, t6, t7, t8, t9]
    '[a1, a2, a3, a4, a5, a6, a7, a8, a9]) a where
  lensOf = lens p9_8 (\p a -> p{ p9_8 = a })
  {-# INLINE lensOf #-}

instance a ~ a9 => HasLens t9
  (Product
    '[t1, t2, t3, t4, t5, t6, t7, t8, t9]
    '[a1, a2, a3, a4, a5, a6, a7, a8, a9]) a where
  lensOf = lens p9_9 (\p a -> p{ p9_9 = a })
  {-# INLINE lensOf #-}
