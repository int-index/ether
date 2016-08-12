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
  , Product
  , load
  , type (++)
  , (<+>)
  ) where

import Control.Lens
import Control.DeepSeq
import GHC.Prim (Proxy#)

class HasLens tag outer inner | tag outer -> inner where
  lensOf :: Proxy# tag -> Lens' outer inner

data Product (ts :: [k]) (as :: [*]) where
  Nil :: Product '[] '[]
  Cons :: a -> !(Product ts as) -> Product (t ': ts) (a ': as)

instance NFData (Product '[] '[]) where
  rnf Nil = ()

instance
    ( NFData a
    , NFData (Product ts as)
    ) => NFData (Product (t ': ts) (a ': as))
  where
    rnf (Cons a as) = rnf a `seq` rnf as

load :: forall tag a . a -> Product '[tag] '[a]
load a = Cons a Nil
{-# INLINE load #-}

type family (xs1 :: [k]) ++ (xs2 :: [k]) :: [k] where
  '[] ++ xs2 = xs2
  (a ': xs1) ++ xs2 = a ': (xs1 ++ xs2)

(<+>)
  :: Product ts1 as1
  -> Product ts2 as2
  -> Product (ts1 ++ ts2) (as1 ++ as2)
Nil <+> as2 = as2
Cons a as1 <+> as2 = Cons a (as1 <+> as2)
{-# INLINE (<+>) #-}

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
    ) => HasLens tag (Product (t ': tags) (b ': as)) a
  where
    lensOf t = productTail . lensOf t
    {-# INLINE lensOf #-}
