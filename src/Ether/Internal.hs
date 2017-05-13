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

instance HasLens t (Tagged t a) a where
  lensOf = \f -> fmap coerce . f . coerce

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

do
  let
    tupCount = 62
    varName k = TH.mkName ('a' : show k)
    tagName k = TH.mkName ('t' : show k)
  for [2..tupCount] $ \n -> do
    let
      tupTy = List.foldl' TH.AppT (TH.ConT (TH.tupleTypeName n)) $
        (\k -> TH.ConT ''Tagged `TH.AppT`
          TH.VarT (tagName k) `TH.AppT`
          TH.VarT (varName k)) <$> [0..n-1]
      tagsList = List.foldr
        (\a b -> TH.PromotedConsT `TH.AppT` a `TH.AppT` b)
        TH.PromotedNilT
        (TH.AppT (TH.ConT ''KindOf) . TH.VarT . tagName <$> [0..n-1])
    return $
      TH.TySynInstD ''TagsK (TH.TySynEqn [tupTy] tagsList)

return []

type instance Tags () = 'HNil
type instance Tags (Tagged t a) = 'HCons t 'HNil

do
  let
    tupCount = 62
    varName k = TH.mkName ('a' : show k)
    tagName k = TH.mkName ('t' : show k)
  for [2..tupCount] $ \n -> do
    let
      tupTy = List.foldl' TH.AppT (TH.ConT (TH.tupleTypeName n)) $
        (\k -> TH.ConT ''Tagged `TH.AppT`
          TH.VarT (tagName k) `TH.AppT`
          TH.VarT (varName k)) <$> [0..n-1]
      tagsList = List.foldr
        (\a b -> TH.PromotedT 'HCons `TH.AppT` a `TH.AppT` b)
        (TH.PromotedT 'HNil)
        (TH.VarT . tagName <$> [0..n-1])
    return $
      TH.TySynInstD ''Tags (TH.TySynEqn [tupTy] tagsList)

do
  let
    tupCount = 62
    varName k = TH.mkName ('a' : show k)
  fmap List.concat $
    for [2..tupCount] $ \n ->
      for [0..n-1] $ \k -> do
        let
          tag = TH.mkName "tag"
          prev = varName <$> [0..k-1]
          cur  = varName k
          next = varName <$> [k+1..n-1]
          tupTy = foldl TH.AppT (TH.ConT (TH.tupleTypeName n))
            ( map TH.VarT prev ++
              [TH.ConT ''Tagged `TH.AppT` TH.VarT tag `TH.AppT` TH.VarT cur] ++
              map TH.VarT next )
        let
          cur' = TH.mkName "x"
          f = TH.mkName "f"
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
