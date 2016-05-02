{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}

module Control.Ether.Flatten
  ( HasLens(..)
  , Product(..)
  , load
  ) where

import Control.Lens
import GHC.Prim (Proxy#)

class HasLens tag outer inner | tag outer -> inner where
  lensOf :: Proxy# tag -> Lens' outer inner

data Product (ts :: [k]) (as :: [*]) where
  Nil :: Product '[] '[]
  Cons :: !a -> !(Product ts as) -> Product (t ': ts) (a ': as)

load
  :: forall tag a tags as
   . a -> Product tags as -> Product (tag ': tags) (a ': as)
load = Cons

productHead ::
  Lens
    (Product (t1 ': ts) (a ': as))
    (Product (t2 ': ts) (b ': as))
    a
    b
productHead =
  lens
    (\(Cons a _) -> a)
    (\(Cons _ as) a -> Cons a as)
{-# INLINE productHead #-}

productTail ::
  Lens
    (Product (t ': ts1) (a ': as))
    (Product (t ': ts2) (a ': bs))
    (Product ts1 as)
    (Product ts2 bs)
productTail =
  lens
    (\(Cons _ as) -> as)
    (\(Cons a _) as -> Cons a as)
{-# INLINE productTail #-}

instance a ~ b => HasLens tag (Product (tag ': tags) (a ': as)) b where
  lensOf _ = productHead
  {-# INLINE lensOf #-}

instance {-# OVERLAPPABLE #-}
         ( HasLens tag (Product tags as) a
         ) => HasLens tag (Product (t ': tags) (b ': as)) a where
  lensOf t = productTail . lensOf t
  {-# INLINE lensOf #-}
