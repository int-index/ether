{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Trans.Ether.State
    ( EtherStateT
    , EtherState
    , StateT'
    , State'
    , runEtherStateT
    , runEtherState
    , runStateT'
    , runState'
    --
    , etherStateT
    , mapEtherStateT
    , liftCatch
    , liftCallCC'
    , liftListen
    , liftPass
    ) where

import Data.Proxy (Proxy(Proxy))
import Data.Functor.Identity (Identity(..))
import Data.Coerce (coerce)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO)
import Control.Ether.Core

import qualified Control.Monad.Signatures as Sig
import qualified Control.Monad.Trans.State.Lazy as S

import qualified Control.Monad.Cont.Class    as Class
import qualified Control.Monad.Reader.Class  as Class
import qualified Control.Monad.State.Class   as Class
import qualified Control.Monad.Writer.Class  as Class
import qualified Control.Monad.Error.Class   as Class

newtype EtherStateT tag s m a = EtherStateT (S.StateT s m a)
    deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
             , MonadFix, MonadTrans, MonadIO )

type instance EtherTags (EtherStateT tag r m) = tag ': EtherTags m

type EtherState tag r = EtherStateT tag r Identity

etherStateT :: proxy tag -> (s -> m (a, s)) -> EtherStateT tag s m a
etherStateT _proxy = EtherStateT . S.StateT

runEtherStateT :: proxy tag -> EtherStateT tag s m a  -> s -> m (a, s)
runEtherStateT _proxy (EtherStateT (S.StateT f)) = f

runEtherState :: proxy tag -> EtherState tag s a  -> s -> (a, s)
runEtherState proxy m s = runIdentity (runEtherStateT proxy m s)

mapEtherStateT :: proxy tag -> (m (a, s) -> n (b, s)) -> EtherStateT tag s m a -> EtherStateT tag s n b
mapEtherStateT _proxy f m = EtherStateT $ S.mapStateT f (coerce m)

liftCatch :: proxy tag -> Sig.Catch e m (a, s) -> Sig.Catch e (EtherStateT tag s m) a
liftCatch _proxy f m h = EtherStateT $ S.liftCatch f (coerce m) (coerce h)

liftCallCC' :: proxy tag -> Sig.CallCC m (a, s) (b, s) -> Sig.CallCC (EtherStateT tag s m) a b
liftCallCC' _proxy callCC f = EtherStateT $ S.liftCallCC' callCC (coerce f)

liftListen :: Monad m => proxy tag -> Sig.Listen w m (a, s) -> Sig.Listen w (EtherStateT tag s m) a
liftListen _proxy listen m = EtherStateT $ S.liftListen listen (coerce m)

liftPass :: Monad m => proxy tag -> Sig.Pass w m (a,s) -> Sig.Pass w (EtherStateT tag s m) a
liftPass _proxy pass m = EtherStateT $ S.liftPass pass (coerce m)

type StateT' s = EtherStateT s s
type State'  s = EtherState  s s

runStateT' :: StateT' s m a -> s -> m (a, s)
runStateT' = runEtherStateT Proxy

runState' :: State' s a -> s -> (a, s)
runState' m s = runIdentity (runStateT' m s)

-- Instances for mtl classes

instance Class.MonadCont m => Class.MonadCont (EtherStateT tag s m) where
    callCC = liftCallCC' Proxy Class.callCC

instance Class.MonadReader r m => Class.MonadReader r (EtherStateT tag s m) where
    ask = lift Class.ask
    local = mapEtherStateT Proxy . Class.local
    reader = lift . Class.reader

instance Class.MonadState s' m => Class.MonadState s' (EtherStateT tag s m) where
    get = lift Class.get
    put = lift . Class.put
    state = lift . Class.state

instance Class.MonadWriter w m => Class.MonadWriter w (EtherStateT tag s m) where
    writer = lift . Class.writer
    tell   = lift . Class.tell
    listen = liftListen Proxy Class.listen
    pass   = liftPass Proxy Class.pass

instance Class.MonadError e m => Class.MonadError e (EtherStateT tag s m) where
    throwError = lift . Class.throwError
    catchError = liftCatch Proxy Class.catchError
