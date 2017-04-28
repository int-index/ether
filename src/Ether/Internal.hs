module Ether.Internal
  ( Tagged(..)
  , TagsK
  , Tags
  , HasLens(..)
  , LensLike
  , Lens
  , Lens'
  , ReifiedLens(..)
  , ReifiedLens'
  , view
  , over
  , HList(..)
  , KindOf
  , TAGGED
  , HandleSuper
  , HandleConstraint
  , Handle(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Coerce
import Data.Functor.Identity
import Data.Kind
import Data.List as List
import Data.Tagged
import Data.Traversable
import GHC.Exts (Constraint)
import qualified Language.Haskell.TH as TH

data TAGGED e t

type K_Monad = Type -> Type

type K_Trans = K_Monad -> K_Monad

type family
  HandleSuper
    (eff :: keff)
    (p :: kp)
    (trans :: K_Trans)
  :: Constraint

type family
  HandleConstraint
    (eff :: keff)
    (p :: kp)
    (trans :: K_Trans) (m :: K_Monad)
  :: Constraint

class
  HandleSuper eff p trans =>
    Handle eff p (trans :: K_Trans) | eff trans -> p
  where
    handling :: Monad m => (HandleConstraint eff p trans m => r) -> r

type LensLike f s t a b = (a -> f b) -> s -> f t

type Lens s t a b = forall f. Functor f => LensLike f s t a b

type Lens' s a = Lens s s a a

newtype ReifiedLens s t a b = Lens (Lens s t a b)

type ReifiedLens' s a = ReifiedLens s s a a

class HasLens tag outer inner | tag outer -> inner where
  lensOf :: Lens' outer inner

instance HasLens a a a where
  lensOf = id
  {-# INLINE lensOf #-}

view :: LensLike (Const a) s t a b -> s -> a
view l = coerce (l Const)
{-# INLINE view #-}

over :: LensLike Identity s t a b -> (a -> b) -> s -> t
over = coerce
{-# INLINE over #-}

data HList xs where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

type KindOf (a :: k) = k

type family TagsK (p :: Type) :: [Type]
type family Tags  (p :: Type) :: HList (TagsK p)

return []

type instance TagsK () = '[]
type instance TagsK (Tagged t a) = '[KindOf t]
type instance TagsK (Tagged t0 a, Tagged t1 b) = '[KindOf t0, KindOf t1]

return []

type instance Tags () = 'HNil
type instance Tags (Tagged t a) = 'HCons t 'HNil
type instance Tags (Tagged t0 a, Tagged t1 b) = 'HCons t0 ('HCons t1 'HNil)

do
  let
    tupCount = 62
    names    = [1..] >>= flip replicateM ['a'..'z']
  varNames <- traverse TH.newName (take tupCount names)
  fmap List.concat $
    for (List.drop 2 (List.inits varNames)) $
      \varNames' -> do
        let n = List.length varNames'
        tagsInstances <- for [() | n > 2] $ \() -> do
          tag <- TH.newName "tag"
          let
            (cur:rest) = varNames'
            tupTy = foldl TH.AppT (TH.ConT (TH.tupleTypeName n))
              ( TH.ConT ''Tagged `TH.AppT` TH.VarT tag `TH.AppT` TH.VarT cur :
                map TH.VarT rest )
            tupTy' = foldl TH.AppT (TH.ConT (TH.tupleTypeName (n-1)))
              (map TH.VarT rest)
          return $
            TH.TySynInstD ''TagsK (TH.TySynEqn [tupTy]
              ( TH.PromotedConsT `TH.AppT` (TH.ConT ''KindOf `TH.AppT` TH.VarT tag) `TH.AppT`
                (TH.ConT ''TagsK `TH.AppT` tupTy') ))
        return tagsInstances
do
  let
    tupCount = 62
    names    = [1..] >>= flip replicateM ['a'..'z']
  varNames <- traverse TH.newName (take tupCount names)
  fmap List.concat $
    for (List.drop 2 (List.inits varNames)) $
      \varNames' -> do
        let n = List.length varNames'
        tagsInstances <- for [() | n > 2] $ \() -> do
          tag <- TH.newName "tag"
          let
            (cur:rest) = varNames'
            tupTy = foldl TH.AppT (TH.ConT (TH.tupleTypeName n))
              ( TH.ConT ''Tagged `TH.AppT` TH.VarT tag `TH.AppT` TH.VarT cur :
                map TH.VarT rest )
            tupTy' = foldl TH.AppT (TH.ConT (TH.tupleTypeName (n-1)))
              (map TH.VarT rest)
            tagsInst =
              TH.TySynInstD ''Tags (TH.TySynEqn [tupTy]
                ( TH.PromotedT 'HCons `TH.AppT` TH.VarT tag `TH.AppT`
                  (TH.ConT ''Tags `TH.AppT` tupTy') ))
          return tagsInst
        hasLensInstances <- for [0..n-1] $ \k -> do
          tag <- TH.newName "tag"
          let
            (prev, cur:next) = List.splitAt k varNames'
            tupTy = foldl TH.AppT (TH.ConT (TH.tupleTypeName n))
              ( map TH.VarT prev ++
                [TH.ConT ''Tagged `TH.AppT` TH.VarT tag `TH.AppT` TH.VarT cur] ++
                map TH.VarT next )
          cur' <- TH.newName "x"
          f <- TH.newName "f"
          return $
            TH.InstanceD Nothing []
              (TH.ConT ''HasLens `TH.AppT` TH.VarT tag `TH.AppT` tupTy `TH.AppT` TH.VarT cur)
              [ TH.FunD 'lensOf
                  [ TH.Clause
                      [TH.VarP f, TH.TupP
                        ( map TH.VarP prev ++
                          [TH.ConP 'Tagged [TH.VarP cur]] ++
                          map TH.VarP next )]
                      (TH.NormalB $
                         TH.VarE 'fmap `TH.AppE`
                           (TH.LamE [TH.VarP cur']
                             (TH.TupE
                              ( map TH.VarE prev ++
                                [TH.ConE 'Tagged `TH.AppE` TH.VarE cur'] ++
                                map TH.VarE next ))) `TH.AppE`
                           (TH.VarE f `TH.AppE` TH.VarE cur) )
                      [] ],
                TH.PragmaD (TH.InlineP 'lensOf TH.Inline TH.FunLike TH.AllPhases) ]
        return $ tagsInstances ++ hasLensInstances
