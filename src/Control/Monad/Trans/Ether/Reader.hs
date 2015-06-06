{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Trans.Ether.Reader where

import Data.Proxy (Proxy(Proxy))
import Data.Coerce (coerce)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO)
import Control.Ether.Core (EtherData)

import qualified Control.Monad.Signatures as Sig
import qualified Control.Monad.Trans.Reader as R

import qualified Control.Monad.Cont.Class    as Class
import qualified Control.Monad.Reader.Class  as Class
import qualified Control.Monad.State.Class   as Class
import qualified Control.Monad.Writer.Class  as Class
import qualified Control.Monad.Error.Class   as Class

newtype EtherReaderT tag m a = EtherReaderT (R.ReaderT (EtherData tag) m a)
    deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
             , MonadFix, MonadTrans, MonadIO )

etherReaderT :: proxy tag -> (EtherData tag -> m a) -> EtherReaderT tag m a
etherReaderT _proxy = EtherReaderT . R.ReaderT

runEtherReaderT :: proxy tag -> EtherReaderT tag m a -> EtherData tag -> m a
runEtherReaderT _proxy (EtherReaderT (R.ReaderT s)) = s

mapEtherReaderT :: proxy tag -> (m a -> n b) -> EtherReaderT tag m a -> EtherReaderT tag n b
mapEtherReaderT _proxy f m = coerce $ R.mapReaderT f (coerce m)

liftCatch :: proxy tag -> Sig.Catch e m a -> Sig.Catch e (EtherReaderT tag m) a
liftCatch _proxy f m h = coerce $ R.liftCatch f (coerce m) (coerce h)

liftCallCC :: proxy tag -> Sig.CallCC m a b -> Sig.CallCC (EtherReaderT tag m) a b
liftCallCC _proxy callCC f = coerce $ R.liftCallCC callCC (coerce f)

-- Instances for mtl classes

instance Class.MonadCont m => Class.MonadCont (EtherReaderT tag m) where
    callCC = liftCallCC Proxy Class.callCC

instance Class.MonadReader r m => Class.MonadReader r (EtherReaderT tag m) where
    ask = lift Class.ask
    local = mapEtherReaderT Proxy . Class.local
    reader = lift . Class.reader

instance Class.MonadState s m => Class.MonadState s (EtherReaderT tag m) where
    get = lift Class.get
    put = lift . Class.put
    state = lift . Class.state

instance Class.MonadWriter w m => Class.MonadWriter w (EtherReaderT tag m) where
    writer = lift . Class.writer
    tell   = lift . Class.tell
    listen = mapEtherReaderT Proxy Class.listen
    pass   = mapEtherReaderT Proxy Class.pass

instance Class.MonadError e m => Class.MonadError e (EtherReaderT tag m) where
    throwError = lift . Class.throwError
    catchError = liftCatch Proxy Class.catchError
