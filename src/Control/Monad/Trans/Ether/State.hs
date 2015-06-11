{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Trans.Ether.State
    ( StateT
    , State
    , runStateT
    , runState
    , evalStateT
    , evalState
    , execStateT
    , execState
    , stateT
    , mapStateT
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
import Control.Ether.Tags (Tags)

import qualified Control.Monad.Signatures as Sig
import qualified Control.Monad.Trans.State.Lazy as Trans

import qualified Control.Monad.Cont.Class    as Class
import qualified Control.Monad.Reader.Class  as Class
import qualified Control.Monad.State.Class   as Class
import qualified Control.Monad.Writer.Class  as Class
import qualified Control.Monad.Error.Class   as Class

newtype StateT tag s m a = StateT (Trans.StateT s m a)
    deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
             , MonadFix, MonadTrans, MonadIO )

type instance Tags (StateT tag r m) = tag ': Tags m

type State tag r = StateT tag r Identity

stateT :: proxy tag -> (s -> m (a, s)) -> StateT tag s m a
stateT _proxy = StateT . Trans.StateT

runStateT :: proxy tag -> StateT tag s m a  -> s -> m (a, s)
runStateT _proxy (StateT (Trans.StateT f)) = f

runState :: proxy tag -> State tag s a  -> s -> (a, s)
runState proxy m s = runIdentity (runStateT proxy m s)

evalStateT :: Functor m => proxy tag -> StateT tag s m a  -> s -> m a
evalStateT proxy m s = fmap fst (runStateT proxy m s)

evalState :: proxy tag -> State tag s a  -> s -> a
evalState proxy m s = fst (runState proxy m s)

execStateT :: Functor m => proxy tag -> StateT tag s m a  -> s -> m s
execStateT proxy m s = fmap snd (runStateT proxy m s)

execState :: proxy tag -> State tag s a  -> s -> s
execState proxy m s = snd (runState proxy m s)

mapStateT :: proxy tag -> (m (a, s) -> n (b, s)) -> StateT tag s m a -> StateT tag s n b
mapStateT _proxy f m = StateT $ Trans.mapStateT f (coerce m)

liftCatch :: proxy tag -> Sig.Catch e m (a, s) -> Sig.Catch e (StateT tag s m) a
liftCatch _proxy f m h = StateT $ Trans.liftCatch f (coerce m) (coerce h)

liftCallCC' :: proxy tag -> Sig.CallCC m (a, s) (b, s) -> Sig.CallCC (StateT tag s m) a b
liftCallCC' _proxy callCC f = StateT $ Trans.liftCallCC' callCC (coerce f)

liftListen :: Monad m => proxy tag -> Sig.Listen w m (a, s) -> Sig.Listen w (StateT tag s m) a
liftListen _proxy listen m = StateT $ Trans.liftListen listen (coerce m)

liftPass :: Monad m => proxy tag -> Sig.Pass w m (a,s) -> Sig.Pass w (StateT tag s m) a
liftPass _proxy pass m = StateT $ Trans.liftPass pass (coerce m)

-- Instances for mtl classes

instance Class.MonadCont m => Class.MonadCont (StateT tag s m) where
    callCC = liftCallCC' Proxy Class.callCC

instance Class.MonadReader r m => Class.MonadReader r (StateT tag s m) where
    ask = lift Class.ask
    local = mapStateT Proxy . Class.local
    reader = lift . Class.reader

instance Class.MonadState s' m => Class.MonadState s' (StateT tag s m) where
    get = lift Class.get
    put = lift . Class.put
    state = lift . Class.state

instance Class.MonadWriter w m => Class.MonadWriter w (StateT tag s m) where
    writer = lift . Class.writer
    tell   = lift . Class.tell
    listen = liftListen Proxy Class.listen
    pass   = liftPass Proxy Class.pass

instance Class.MonadError e m => Class.MonadError e (StateT tag s m) where
    throwError = lift . Class.throwError
    catchError = liftCatch Proxy Class.catchError
