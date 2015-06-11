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
    , runReader
    -- * The ReaderT monad transformer
    , ReaderT
    , readerT
    , runReaderT
    , mapReaderT
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

type instance Tags (ReaderT tag r m) = tag ': Tags m

-- | Constructor for computations in the reader monad transformer.
readerT :: proxy tag -> (r -> m a) -> ReaderT tag r m a
readerT _proxy = ReaderT . Trans.ReaderT

-- | Runs a 'Reader' and extracts the final value from it.
runReaderT :: proxy tag -> ReaderT tag r m a -> r -> m a
runReaderT _proxy (ReaderT (Trans.ReaderT f)) = f

-- | Runs a 'Reader' and extracts the final value from it.
runReader :: proxy tag -> Reader tag r a -> r -> a
runReader proxy m r = runIdentity (runReaderT proxy m r)

-- | Transform the computation inside a @ReaderT@.
--
-- * @'runReaderT' tag ('mapReaderT' tag f m) = f . 'runReaderT' tag m@
mapReaderT :: proxy tag -> (m a -> n b) -> ReaderT tag r m a -> ReaderT tag r n b
mapReaderT _proxy f m = ReaderT $ Trans.mapReaderT f (coerce m)

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: proxy tag -> Sig.Catch e m a -> Sig.Catch e (ReaderT tag r m) a
liftCatch _proxy f m h = ReaderT $ Trans.liftCatch f (coerce m) (coerce h)

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: proxy tag -> Sig.CallCC m a b -> Sig.CallCC (ReaderT tag r m) a b
liftCallCC _proxy callCC f = ReaderT $ Trans.liftCallCC callCC (coerce f)

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
