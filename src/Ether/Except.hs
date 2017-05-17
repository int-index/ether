module Ether.Except
  (
  -- * MonadExcept class
    MonadExcept
  , throw
  , catch
  -- * The Except monad
  , Except
  , runExcept
  -- * The ExceptT monad transformer
  , ExceptT
  , exceptT
  , runExceptT
  -- * MonadExcept class (implicit)
  , MonadExcept'
  , throw'
  , catch'
  -- * The Except monad (implicit)
  , Except'
  , runExcept'
  -- * The ExceptT monad transformer (implicit)
  , ExceptT'
  , exceptT'
  , runExceptT'
  -- * Internal labels
  , TAGGED
  , EXCEPT
  ) where

import qualified Control.Monad.Except as T
import Control.Monad.Signatures (Catch)
import qualified Control.Monad.Trans.Lift.Catch as Lift
import Data.Coerce
import Data.Functor.Identity

import Ether.TaggedTrans
import Ether.Internal

class Monad m => MonadExcept tag e m | m tag -> e where

    -- | Is used within a monadic computation to begin exception processing.
    throw :: e -> m a

    -- | A TaggedTrans function to handle previous exceptions and return to
    -- normal execution.
    catch :: m a -> (e -> m a) -> m a

instance {-# OVERLAPPABLE #-}
         ( Lift.LiftCatch t
         , Monad (t m)
         , MonadExcept tag e m
         ) => MonadExcept tag e (t m) where
    throw = Lift.lift . throw @tag
    catch = Lift.liftCatch (catch @tag)

-- | Encode type-level information for 'ExceptT'.
data EXCEPT

-- | The parameterizable exception monad.
--
-- Computations are either exceptions or normal values.
--
-- The 'return' function returns a normal value, while '>>=' exits on
-- the first exception.
type Except tag e = ExceptT tag e Identity

-- | The exception monad transformer.
--
-- The 'return' function returns a normal value, while '>>=' exits on
-- the first exception.
type ExceptT tag e = TaggedTrans (TAGGED EXCEPT tag) (T.ExceptT e)

-- | Runs an 'Except' and returns either an exception or a normal value.
runExcept :: forall tag e a . Except tag e a -> Either e a
runExcept = coerce (T.runExcept @e @a)

-- | Runs an 'ExceptT' and returns either an exception or a normal value.
runExceptT :: forall tag e m a . ExceptT tag e m a -> m (Either e a)
runExceptT = coerce (T.runExceptT @e @m @a)

-- | Constructor for computations in the exception monad transformer.
exceptT :: forall tag e m a . m (Either e a) -> ExceptT tag e m a
exceptT = coerce (T.ExceptT @e @m @a)

type instance HandleSuper      EXCEPT e trans   = ()
type instance HandleConstraint EXCEPT e trans m =
  T.MonadError e (trans m)

instance Handle EXCEPT e (T.ExceptT e) where
  handling r = r

instance
    ( Handle EXCEPT e trans
    , Monad m, Monad (trans m)
    ) => MonadExcept tag e (TaggedTrans (TAGGED EXCEPT tag) trans m)
  where

    throw =
      handling @EXCEPT @e @trans @m $
      coerce (T.throwError @e @(trans m) @a) ::
        forall eff a . e -> TaggedTrans eff trans m a

    catch =
      handling @EXCEPT @e @trans @m $
      coerce (T.catchError @e @(trans m) @a) ::
        forall eff a . Catch e (TaggedTrans eff trans m) a

type MonadExcept' e = MonadExcept e e

throw' :: forall e m a . MonadExcept' e m => e -> m a
throw' = throw @e

catch' :: forall e m a . MonadExcept' e m => m a -> (e -> m a) -> m a
catch' = catch @e

type Except' e = Except e e

runExcept' :: Except' e a -> Either e a
runExcept' = runExcept

type ExceptT' e = ExceptT e e

exceptT' :: m (Either e a) -> ExceptT' e m a
exceptT' = exceptT

runExceptT' :: ExceptT' e m a -> m (Either e a)
runExceptT' = runExceptT
