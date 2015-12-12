{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | See "Control.Monad.Trans.Reader".

module Control.Monad.Trans.Ether.Reader
    (
    -- * The Reader monad
      Reader
    , reader
    , runReader
    -- * The ReaderT monad transformer
    , ReaderT
    , readerT
    , runReaderT
    -- * Reader operations
    , ask
    , local
    -- * Newtype operations
    , pack
    , unpack
    ) where

import Data.Functor.Identity (Identity(..))
import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (MFunctor, MMonad)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import GHC.Generics (Generic)
import Data.Coerce (coerce)

import qualified Control.Monad.Base as MB
import qualified Control.Monad.Trans.Control as MC
import qualified Control.Monad.Trans.Reader as Trans

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

-- | The parameterizable reader monad.
--
-- Computations are functions of a shared environment.
--
-- The 'return' function ignores the environment, while '>>=' passes
-- the inherited environment to both subcomputations.
type Reader tag r = ReaderT tag r Identity

-- | The reader monad transformer,
-- which adds a read-only environment to the given monad.
--
-- The 'return' function ignores the environment, while '>>=' passes
-- the inherited environment to both subcomputations.
newtype ReaderT tag r m a = ReaderT (Trans.ReaderT r m a)
    deriving ( Generic
             , Functor, Applicative, Alternative, Monad, MonadPlus
             , MonadFix, MonadTrans, MonadIO, MFunctor, MMonad
             , MonadThrow, MonadCatch, MonadMask )

-- | Type-restricted 'coerce'.
pack :: Trans.ReaderT r m a -> ReaderT tag r m a
pack = coerce

-- | Type-restricted 'coerce'.
unpack :: ReaderT tag r m a -> Trans.ReaderT r m a
unpack = coerce

instance MB.MonadBase b m => MB.MonadBase b (ReaderT tag r m) where
    liftBase = MB.liftBaseDefault

instance MC.MonadTransControl (ReaderT tag r) where
    type StT (ReaderT tag r) a = MC.StT (Trans.ReaderT r) a
    liftWith = MC.defaultLiftWith pack unpack
    restoreT = MC.defaultRestoreT pack

instance MC.MonadBaseControl b m => MC.MonadBaseControl b (ReaderT tag r m) where
    type StM (ReaderT tag r m) a = MC.ComposeSt (ReaderT tag r) m a
    liftBaseWith = MC.defaultLiftBaseWith
    restoreM = MC.defaultRestoreM

type instance Lift.StT (ReaderT tag r) a = MC.StT (ReaderT tag r) a

instance Lift.LiftLocal (ReaderT tag r) where
    liftLocal = Lift.defaultLiftLocal pack unpack

instance Lift.LiftCatch (ReaderT tag r) where
    liftCatch = Lift.defaultLiftCatch pack unpack

instance Lift.LiftListen (ReaderT tag r) where
    liftListen = Lift.defaultLiftListen pack unpack

instance Lift.LiftPass (ReaderT tag r) where
    liftPass = Lift.defaultLiftPass pack unpack

instance Lift.LiftCallCC (ReaderT tag r) where
    liftCallCC  = Lift.defaultLiftCallCC  pack unpack
    liftCallCC' = Lift.defaultLiftCallCC' pack unpack

tagged :: proxy tag -> Trans.ReaderT r m a -> ReaderT tag r m a
tagged _ = pack

untagged :: proxy tag -> ReaderT tag r m a -> Trans.ReaderT r m a
untagged _ = unpack

-- | Constructor for computations in the reader monad transformer.
readerT :: proxy tag -> (r -> m a) -> ReaderT tag r m a
readerT t = tagged t . Trans.ReaderT

-- | Constructor for computations in the reader monad
-- (the inverse of 'runReader').
reader :: Monad m => proxy tag -> (r -> a) -> ReaderT tag r m a
reader t = tagged t . Trans.reader

-- | Runs a 'ReaderT' with the given environment
-- and returns the vinal value.
runReaderT :: proxy tag -> ReaderT tag r m a -> r -> m a
runReaderT t = Trans.runReaderT . untagged t

-- | Runs a 'ReaderT' with the given environment
-- and returns the vinal value.
runReader :: proxy tag -> Reader tag r a -> r -> a
runReader t = Trans.runReader . untagged t

-- | Fetch the value of the environment.
ask :: Monad m => proxy tag -> ReaderT tag r m r
ask t = tagged t Trans.ask

-- | Execute a computation in a modified environment
-- (a specialization of 'withReaderT').
--
-- * @'runReaderT' tag ('local' tag f m) = 'runReaderT' tag m . f@
local
    :: proxy tag
    -> (r -> r)
    -- ^ The function to modify the environment.
    -> ReaderT tag r m a
    -- ^ Computation to run in the modified environment.
    -> ReaderT tag r m a
local t f m = tagged t $ Trans.withReaderT f (untagged t m)

-- Instances for mtl classes

instance Class.MonadCont m => Class.MonadCont (ReaderT tag r m) where
    callCC = Lift.liftCallCC Class.callCC

instance Class.MonadReader r' m => Class.MonadReader r' (ReaderT tag r m) where
    ask = lift Class.ask
    local = Lift.liftLocal Class.ask Class.local
    reader = lift . Class.reader

instance Class.MonadState s m => Class.MonadState s (ReaderT tag r m) where
    get = lift Class.get
    put = lift . Class.put
    state = lift . Class.state

instance Class.MonadWriter w m => Class.MonadWriter w (ReaderT tag r m) where
    writer = lift . Class.writer
    tell   = lift . Class.tell
    listen = Lift.liftListen Class.listen
    pass   = Lift.liftPass Class.pass

instance Class.MonadError e m => Class.MonadError e (ReaderT tag r m) where
    throwError = lift . Class.throwError
    catchError = Lift.liftCatch Class.catchError
