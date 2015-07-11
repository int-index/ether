{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | See "Control.Monad.Trans.Except".

module Control.Monad.Trans.Ether.Except
    (
    -- * The Except monad
      Except
    , except
    , runExcept
    -- * The ExceptT monad transformer
    , ExceptT
    , exceptT
    , runExceptT
    -- * Exception operations
    , throw
    , catch
    ) where

import Data.Functor.Identity (Identity(..))
import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (MFunctor, MMonad)
import Control.Ether.Tagged (Taggable(..), Tagged(..))
import GHC.Generics (Generic)
import qualified Control.Newtype as NT

import qualified Control.Monad.Trans.Control as MC
import qualified Control.Monad.Trans.Except as Trans

import qualified Control.Monad.Trans.Lift.StT    as Lift
import qualified Control.Monad.Trans.Lift.Local  as Lift
import qualified Control.Monad.Trans.Lift.Catch  as Lift
import qualified Control.Monad.Trans.Lift.Listen as Lift
import qualified Control.Monad.Trans.Lift.Pass   as Lift
import qualified Control.Monad.Trans.Lift.CallCC as Lift

import qualified Control.Monad.Cont.Class    as Class
import qualified Control.Monad.Reader.Class  as Class
import qualified Control.Monad.State.Class   as Class
import qualified Control.Monad.Writer.Class  as Class
import qualified Control.Monad.Error.Class   as Class


-- | The parameterizable exception monad.
--
-- Computations are either exceptions or normal values.
--
-- The 'return' function returns a normal value, while '>>=' exits on
-- the first exception.
type Except tag e = ExceptT tag e Identity

-- | Runs an 'Except' and returns either an exception or a normal value.
runExcept :: proxy tag -> Except tag e a -> Either e a
runExcept t = Trans.runExcept . untagged t

-- | The exception monad transformer.
--
-- The 'return' function returns a normal value, while '>>=' exits on
-- the first exception.
newtype ExceptT tag e m a = ExceptT (Trans.ExceptT e m a)
    deriving ( Generic
             , Functor, Applicative, Alternative, Monad, MonadPlus
             , MonadFix, MonadTrans, MonadIO, MFunctor, MMonad )

instance NT.Newtype (ExceptT tag e m a)

instance MC.MonadTransControl (ExceptT tag e) where
    type StT (ExceptT tag e) a = MC.StT (Trans.ExceptT e) a
    liftWith = MC.defaultLiftWith NT.pack NT.unpack
    restoreT = MC.defaultRestoreT NT.pack

type instance Lift.StT (ExceptT tag e) a = MC.StT (ExceptT tag e) a

instance Lift.LiftLocal (ExceptT tag e) where
    liftLocal = Lift.defaultLiftLocal NT.pack NT.unpack

instance Lift.LiftCatch (ExceptT tag e) where
    liftCatch = Lift.defaultLiftCatch NT.pack NT.unpack

instance Lift.LiftListen (ExceptT tag e) where
    liftListen = Lift.defaultLiftListen NT.pack NT.unpack

instance Lift.LiftPass (ExceptT tag e) where
    liftPass = Lift.defaultLiftPass NT.pack NT.unpack

instance Lift.LiftCallCC (ExceptT tag e) where
    liftCallCC  = Lift.defaultLiftCallCC NT.pack NT.unpack
    liftCallCC' = Lift.defaultLiftCallCC NT.pack NT.unpack

instance Taggable (ExceptT tag e m) where
    type Tag (ExceptT tag e m) = 'Just tag
    type Inner (ExceptT tag e m) = 'Just m

instance Tagged (ExceptT tag e m) tag where
    type Untagged (ExceptT tag e m) = Trans.ExceptT e m

-- | Constructor for computations in the exception monad transformer.
exceptT :: proxy tag -> m (Either e a) -> ExceptT tag e m a
exceptT t = tagged t . Trans.ExceptT

-- | Constructor for computations in the exception monad
-- (the inverse of 'runExcept').
except :: Monad m => proxy tag -> Either e a -> ExceptT tag e m a
except t = exceptT t . return

-- | Runs an 'ExceptT' and returns either an exception or a normal value.
runExceptT :: proxy tag -> ExceptT tag e m a -> m (Either e a)
runExceptT t = Trans.runExceptT . untagged t

-- | Is used within a monadic computation to begin exception processing.
throw :: Monad m => proxy tag -> e -> ExceptT tag e m a
throw t = tagged t . Trans.throwE

-- | A handler function to handle previous exceptions and return to normal execution.
catch :: Monad m => proxy tag -> ExceptT tag e m a -> (e -> ExceptT tag e m a) -> ExceptT tag e m a
catch t m h = tagged t $ Trans.catchE (untagged t m) (untagged t . h)

instance Class.MonadCont m => Class.MonadCont (ExceptT tag e m) where
    callCC = Lift.liftCallCC Class.callCC

instance Class.MonadReader r m => Class.MonadReader r (ExceptT tag e m) where
    ask = lift Class.ask
    local = Lift.liftLocal Class.ask Class.local
    reader = lift . Class.reader

instance Class.MonadState s m => Class.MonadState s (ExceptT tag e m) where
    get = lift Class.get
    put = lift . Class.put
    state = lift . Class.state

instance Class.MonadWriter w m => Class.MonadWriter w (ExceptT tag e m) where
    writer = lift . Class.writer
    tell   = lift . Class.tell
    listen = Lift.liftListen Class.listen
    pass   = Lift.liftPass Class.pass

instance Class.MonadError e' m => Class.MonadError e' (ExceptT tag e m) where
    throwError = lift . Class.throwError
    catchError = Lift.liftCatch Class.catchError
