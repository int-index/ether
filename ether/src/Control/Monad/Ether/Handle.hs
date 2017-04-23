{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Monad.Ether.Handle
  ( Handle(..)
  , HandleSuper
  , HandleConstraint
  , TAGGED
  ) where

import GHC.Exts (Constraint)

data TAGGED e t

type family
  HandleSuper
    (eff :: keff)
    (p :: kp)
    (trans :: (* -> *) -> * -> *)
  :: Constraint

type family
  HandleConstraint
    (eff :: keff)
    (p :: kp)
    (trans :: (* -> *) -> * -> *) (m :: * -> *)
  :: Constraint

class
  HandleSuper eff p trans =>
    Handle eff p (trans :: (* -> *) -> * -> *) | eff trans -> p
  where
    handling :: Monad m => (HandleConstraint eff p trans m => r) -> r
