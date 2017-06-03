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
  , makeTupleInstancesHasLens
  ) where

import Control.Applicative
import Data.Coerce
import Data.Functor.Identity
import Data.Kind
import Data.Tagged
import GHC.Exts (Constraint)

import Ether.Internal.HasLens
import Ether.Internal.Tags
import Ether.Internal.TH_TupleInstances (makeTupleInstancesHasLens)
import Ether.Internal.TupleInstances ()

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

newtype ReifiedLens s t a b = Lens (Lens s t a b)

type ReifiedLens' s a = ReifiedLens s s a a

view :: LensLike (Const a) s t a b -> s -> a
view l = coerce (l Const)

over :: LensLike Identity s t a b -> (a -> b) -> s -> t
over = coerce
