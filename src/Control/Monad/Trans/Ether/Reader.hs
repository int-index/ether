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
    -- * Wrapping operations
    , tagWrap
    , tagUnwrap
    ) where

import Data.Proxy (Proxy(Proxy))
import Data.Functor.Identity (Identity(..))
import Data.Coerce (coerce)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO)
import Control.Ether.Tags (Tags)

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
    deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
             , MonadFix, MonadTrans, MonadIO )

tagWrap :: proxy tag -> Trans.ReaderT r m a -> ReaderT tag r m a
tagWrap _ = ReaderT

tagUnwrap :: proxy tag -> ReaderT tag r m a -> Trans.ReaderT r m a
tagUnwrap _ (ReaderT a) = a

type instance Tags (ReaderT tag r m) = tag ': Tags m

-- | Constructor for computations in the reader monad transformer.
readerT :: proxy tag -> (r -> m a) -> ReaderT tag r m a
readerT t = tagWrap t . Trans.ReaderT

-- | Constructor for computations in the reader monad
-- (the inverse of 'runReader').
reader :: Monad m => proxy tag -> (r -> a) -> ReaderT tag r m a
reader t = tagWrap t . Trans.reader

-- | Runs a 'ReaderT' with the given environment
-- and returns the vinal value.
runReaderT :: proxy tag -> ReaderT tag r m a -> r -> m a
runReaderT t = Trans.runReaderT . tagUnwrap t

-- | Runs a 'ReaderT' with the given environment
-- and returns the vinal value.
runReader :: proxy tag -> Reader tag r a -> r -> a
runReader t = Trans.runReader . tagUnwrap t

-- | Transform the computation inside a 'ReaderT'.
--
-- * @'runReaderT' tag ('mapReaderT' tag f m) = f . 'runReaderT' tag m@
mapReaderT :: proxy tag -> (m a -> n b) -> ReaderT tag r m a -> ReaderT tag r n b
mapReaderT t f m = tagWrap t $ Trans.mapReaderT f (coerce m)

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
withReaderT t f m = tagWrap t $ Trans.withReaderT f (coerce m)

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: proxy tag -> Sig.Catch e m a -> Sig.Catch e (ReaderT tag r m) a
liftCatch t f m h = tagWrap t $ Trans.liftCatch f (coerce m) (coerce h)

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: proxy tag -> Sig.CallCC m a b -> Sig.CallCC (ReaderT tag r m) a b
liftCallCC t callCC f = tagWrap t $ Trans.liftCallCC callCC (coerce f)

-- | Fetch the value of the environment.
ask :: Monad m => proxy tag -> ReaderT tag r m r
ask t = tagWrap t Trans.ask

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
