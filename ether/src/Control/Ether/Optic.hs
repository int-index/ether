module Control.Ether.Optic
  ( Tagged(..)
  , Tags
  , HasLens(..)
  , view
  , over
  , HList(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.Kind as K
import Data.List as List
import Data.Tagged
import Data.Traversable
import Language.Haskell.TH
import Data.Coerce

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

class HasLens tag outer inner | tag outer -> inner where
  lensOf :: Lens' outer inner

instance HasLens a a a where
  lensOf = id
  {-# INLINE lensOf #-}

(#.) :: Coercible c b => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b

infixr 9 #.

view :: Lens s t a b -> s -> a
view l = getConst #. l Const
{-# INLINE view #-}

over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity #. l (Identity #. f)
{-# INLINE over #-}

data HList xs where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

type KindOf (a :: k) = k

type family TagsK (p :: K.Type) :: [K.Type]
type family Tags  (p :: K.Type) :: HList (TagsK p)

return []

type instance TagsK () = '[]
type instance TagsK (Tagged t0 a, Tagged t1 b) = '[KindOf t0, KindOf t1]

return []

type instance Tags () = 'HNil
type instance Tags (Tagged t0 a, Tagged t1 b) = 'HCons t0 ('HCons t1 'HNil)

do
  let
    tupCount = 62
    names    = [1..] >>= flip replicateM ['a'..'z']
  varNames <- traverse newName (take tupCount names)
  fmap List.concat $
    for (List.drop 2 (List.inits varNames)) $
      \varNames' -> do
        let n = List.length varNames'
        tagsInstances <- for [() | n > 2] $ \() -> do
          tag <- newName "tag"
          let
            (cur:rest) = varNames'
            tupTy = foldl AppT (ConT (tupleTypeName n))
              ( ConT ''Tagged `AppT` VarT tag `AppT` VarT cur :
                map VarT rest )
            tupTy' = foldl AppT (ConT (tupleTypeName (n-1)))
              (map VarT rest)
          return $
            TySynInstD ''TagsK (TySynEqn [tupTy]
              ( PromotedConsT `AppT` (ConT ''KindOf `AppT` VarT tag) `AppT`
                (ConT ''TagsK `AppT` tupTy') ))
        return tagsInstances
do
  let
    tupCount = 62
    names    = [1..] >>= flip replicateM ['a'..'z']
  varNames <- traverse newName (take tupCount names)
  fmap List.concat $
    for (List.drop 2 (List.inits varNames)) $
      \varNames' -> do
        let n = List.length varNames'
        tagsInstances <- for [() | n > 2] $ \() -> do
          tag <- newName "tag"
          let
            (cur:rest) = varNames'
            tupTy = foldl AppT (ConT (tupleTypeName n))
              ( ConT ''Tagged `AppT` VarT tag `AppT` VarT cur :
                map VarT rest )
            tupTy' = foldl AppT (ConT (tupleTypeName (n-1)))
              (map VarT rest)
            tagsInst =
              TySynInstD ''Tags (TySynEqn [tupTy]
                ( PromotedT 'HCons `AppT` VarT tag `AppT`
                  (ConT ''Tags `AppT` tupTy') ))
          return tagsInst
        hasLensInstances <- for [0..n-1] $ \k -> do
          tag <- newName "tag"
          let
            (prev, cur:next) = List.splitAt k varNames'
            tupTy = foldl AppT (ConT (tupleTypeName n))
              ( map VarT prev ++
                [ConT ''Tagged `AppT` VarT tag `AppT` VarT cur] ++
                map VarT next )
          cur' <- newName "x"
          f <- newName "f"
          return $
            InstanceD Nothing []
              (ConT ''HasLens `AppT` VarT tag `AppT` tupTy `AppT` VarT cur)
              [ FunD 'lensOf
                  [ Clause
                      [VarP f, TupP
                        ( map VarP prev ++
                          [ConP 'Tagged [VarP cur]] ++
                          map VarP next )]
                      (NormalB $
                         VarE 'fmap `AppE`
                           (LamE [VarP cur']
                             (TupE
                              ( map VarE prev ++
                                [ConE 'Tagged `AppE` VarE cur'] ++
                                map VarE next ))) `AppE`
                           (VarE f `AppE` VarE cur) )
                      [] ],
                PragmaD (InlineP 'lensOf Inline FunLike AllPhases) ]
        return $ tagsInstances ++ hasLensInstances
