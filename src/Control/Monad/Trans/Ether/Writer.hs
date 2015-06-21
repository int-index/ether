{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | See "Control.Monad.Trans.Writer".

module Control.Monad.Trans.Ether.Writer
    (
    -- * The Writer monad
      Writer
    , writer
    , runWriter
    , execWriter
    -- * The WriterT monad transformer
    , WriterT
    , writerT
    , runWriterT
    , execWriterT
    -- * Writer operations
    , tell
    , listen
    , pass
    ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

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
import qualified Control.Monad.Trans.Writer.Lazy as Trans

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

-- | The parametrizable writer monad.
--
-- Computations can accumulate a monoid value.
--
-- The 'return' function produces the output 'mempty', while '>>=' combines
-- the outputs of the subcomputations using 'mappend'.
type Writer tag w = WriterT tag w Identity

-- | The writer monad transformer.
--
-- The 'return' function produces the output 'mempty', while '>>=' combines
-- the outputs of the subcomputations using 'mappend'.
newtype WriterT tag w m a = WT { runWT :: Trans.WriterT w m a }
    deriving ( Generic
             , Functor, Applicative, Alternative, Monad, MonadPlus
             , MonadFix, MonadTrans, MonadIO, MFunctor, MMonad )

instance NT.Newtype (WriterT tag w m a)

instance Monoid w => MC.MonadTransControl (WriterT tag w) where
    type StT (WriterT tag w) a = MC.StT (Trans.WriterT w) a
    liftWith = MC.defaultLiftWith WT runWT
    restoreT = MC.defaultRestoreT WT

instance Monoid w => Lift.LiftLocal  (WriterT tag w)
instance Monoid w => Lift.LiftCatch  (WriterT tag w)
instance Monoid w => Lift.LiftListen (WriterT tag w)

instance Monoid w' => Lift.LiftPass (WriterT tag w') where
    liftPass pass' m = WT $ Lift.liftPass pass' (runWT m)

instance Monoid w => Lift.LiftCallCC (WriterT tag w) where
    liftCallCC callCC f = WT $ Lift.liftCallCC callCC (\g -> (runWT . f) (WT . g))

instance Taggable (WriterT tag w m) where
    type Tag (WriterT tag w m) = 'Just tag
    type Inner (WriterT tag w m) = 'Just m

instance Tagged (WriterT tag w m) tag where
    type Untagged (WriterT tag w m) = Trans.WriterT w m

-- | Constructor for computations in the writer monad transformer.
writerT :: proxy tag -> m (a, w) -> WriterT tag w m a
writerT t = tagged t . Trans.WriterT

-- | Constructor for computations in the writer monad
-- (the inverse of 'runWriter').
writer :: Monad m => proxy tag -> (a, w) -> WriterT tag w m a
writer t = tagged t . Trans.writer

-- | Runs a 'WriterT' and returns both the normal value
-- and the final accumulator.
runWriterT :: proxy tag -> WriterT tag w m a -> m (a, w)
runWriterT t = Trans.runWriterT . untagged t

-- | Runs a 'Writer' and returns both the normal value
-- and the final accumulator.
runWriter :: proxy tag -> Writer tag w a -> (a, w)
runWriter t = Trans.runWriter . untagged t

-- | Runs a 'WriterT' and returns the final accumulator,
-- discarding the normal value.
execWriterT :: Monad m => proxy tag -> WriterT tag w m a -> m w
execWriterT t = Trans.execWriterT . untagged t

-- | Runs a 'Writer' and returns the final accumulator,
-- discarding the normal value.
execWriter :: proxy tag -> Writer tag w a -> w
execWriter t = Trans.execWriter . untagged t

-- | Appends a value to the accumulator within the monad.
tell :: Monad m => proxy tag -> w -> WriterT tag w m ()
tell t w = writer t ((), w)

-- | Executes an action and adds its accumulator to the value of the computation.
listen :: (Monoid w, Monad m) => proxy tag -> WriterT tag w m a -> WriterT tag w m (a, w)
listen t m = tagged t $ Trans.listen (untagged t m)

-- | Executes an action which returns a value and a function, and returns the
-- value, applying the function to the accumulator.
pass :: (Monoid w, Monad m) => proxy tag -> WriterT tag w m (a, w -> w) -> WriterT tag w m a
pass t m = tagged t $ Trans.pass (untagged t m)

instance (Monoid w, Class.MonadCont m) => Class.MonadCont (WriterT tag w m) where
    callCC = Lift.liftCallCC Class.callCC

instance (Monoid w, Class.MonadReader r m) => Class.MonadReader r (WriterT tag w m) where
    ask = lift Class.ask
    local = Lift.liftLocal Class.ask Class.local
    reader = lift . Class.reader

instance (Monoid w, Class.MonadState s m) => Class.MonadState s (WriterT tag w m) where
    get = lift Class.get
    put = lift . Class.put
    state = lift . Class.state

instance (Monoid w, Class.MonadWriter w' m) => Class.MonadWriter w' (WriterT tag w m) where
    writer = lift . Class.writer
    tell   = lift . Class.tell
    listen = Lift.liftListen Class.listen
    pass   = Lift.liftPass Class.pass

instance (Monoid w, Class.MonadError e m) => Class.MonadError e (WriterT tag w m) where
    throwError = lift . Class.throwError
    catchError = Lift.liftCatch Class.catchError
