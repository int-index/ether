{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , mapReaderT
    , withReaderT
    -- * Reader operations
    , ask
    , local
    -- * Lifting other operations
    , liftCatch
    , liftCallCC
    ) where

import Data.Proxy (Proxy(Proxy))
import Data.Functor.Identity (Identity(..))
import Data.Coerce (coerce)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO)
import Control.Ether.Tags (Taggable(..), Tagged(..))
import GHC.Generics (Generic)
import qualified Control.Newtype as NT

import qualified Control.Monad.Signatures as Sig
import qualified Control.Monad.Trans.Reader as Trans

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
             , MonadFix, MonadTrans, MonadIO )

instance NT.Newtype (ReaderT tag r m a)

instance Taggable (ReaderT tag r m) where
    type Tag (ReaderT tag r m) = 'Just tag
    type Inner (ReaderT tag r m) = 'Just m

instance Tagged (ReaderT tag r m) tag where
    type Untagged (ReaderT tag r m) = Trans.ReaderT r m

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

-- | Transform the computation inside a 'ReaderT'.
--
-- * @'runReaderT' tag ('mapReaderT' tag f m) = f . 'runReaderT' tag m@
mapReaderT :: proxy tag -> (m a -> n b) -> ReaderT tag r m a -> ReaderT tag r n b
mapReaderT t f m = tagged t $ Trans.mapReaderT f (coerce m)

-- | Execute a computation in a modified environment
-- (a more general version of 'local').
--
-- * @'runReaderT' tag ('withReaderT' tag f m) = 'runReaderT' tag m . f@
withReaderT
    :: proxy tag
    -> (r' -> r)
    -- ^ The function to modify the environment.
    -> ReaderT tag r  m a
    -- ^ Computation to run in the modified environment.
    -> ReaderT tag r' m a
withReaderT t f m = tagged t $ Trans.withReaderT f (coerce m)

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: proxy tag -> Sig.Catch e m a -> Sig.Catch e (ReaderT tag r m) a
liftCatch t f m h = tagged t $ Trans.liftCatch f (coerce m) (coerce h)

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: proxy tag -> Sig.CallCC m a b -> Sig.CallCC (ReaderT tag r m) a b
liftCallCC t callCC f = tagged t $ Trans.liftCallCC callCC (coerce f)

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
local = withReaderT

-- Instances for mtl classes

instance Class.MonadCont m => Class.MonadCont (ReaderT tag r m) where
    callCC = liftCallCC Proxy Class.callCC

instance Class.MonadReader r' m => Class.MonadReader r' (ReaderT tag r m) where
    ask = lift Class.ask
    local = mapReaderT Proxy . Class.local
    reader = lift . Class.reader

instance Class.MonadState s m => Class.MonadState s (ReaderT tag r m) where
    get = lift Class.get
    put = lift . Class.put
    state = lift . Class.state

instance Class.MonadWriter w m => Class.MonadWriter w (ReaderT tag r m) where
    writer = lift . Class.writer
    tell   = lift . Class.tell
    listen = mapReaderT Proxy Class.listen
    pass   = mapReaderT Proxy Class.pass

instance Class.MonadError e m => Class.MonadError e (ReaderT tag r m) where
    throwError = lift . Class.throwError
    catchError = liftCatch Proxy Class.catchError
