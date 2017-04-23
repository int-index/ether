module Control.Monad.Ether.Handle
  ( Handle(..)
  , HandleSuper
  , HandleConstraint
  , TAGGED
  ) where

import Data.Kind as K
import GHC.Exts (Constraint)

data TAGGED e t

type K_Monad = K.Type -> K.Type

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
