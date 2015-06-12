{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Trans.Ether.State
    (
    -- * The State monad
      State
    , state
    , runState
    , evalState
    , execState
    -- * The StateT monad transformer
    , StateT
    , stateT
    , runStateT
    , evalStateT
    , execStateT
    , mapStateT
    -- * State operations
    , get
    , put
    -- * Litfing other operations
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
import Control.Ether.Tags (Taggable(..), Tagged(..), Tags)

import qualified Control.Monad.Signatures as Sig
import qualified Control.Monad.Trans.State.Lazy as Trans

import qualified Control.Monad.Cont.Class    as Class
import qualified Control.Monad.Reader.Class  as Class
import qualified Control.Monad.State.Class   as Class
import qualified Control.Monad.Writer.Class  as Class
import qualified Control.Monad.Error.Class   as Class


-- | The parametrizable state monad.
--
-- Computations have access to a mutable state.
--
-- The 'return' function leaves the state unchanged, while '>>=' uses
-- the final state of the first computation as the initial state of
-- the second.
type State tag r = StateT tag r Identity

-- | The state monad transformer.
--
-- The 'return' function leaves the state unchanged, while '>>=' uses
-- the final state of the first computation as the initial state of
-- the second.
newtype StateT tag s m a = StateT (Trans.StateT s m a)
    deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
             , MonadFix, MonadTrans, MonadIO )


instance Taggable (StateT tag s m) where
    type Tag (StateT tag s m) = 'Just tag
    type Tags' (StateT tag s m) = Tags m

instance Tagged (StateT tag s m) tag where
    type Untagged (StateT tag s m) = Trans.StateT s m
    tagged _ = StateT
    untagged _ (StateT a) = a

-- | Constructor for computations in the state monad transformer.
stateT :: proxy tag -> (s -> m (a, s)) -> StateT tag s m a
stateT t = tagged t . Trans.StateT

-- | Constructor for computations in the state monad
-- (the inverse of 'runState').
state :: Monad m => proxy tag -> (s -> (a, s)) -> StateT tag s m a
state t = tagged t . Trans.state

-- | Runs a 'StateT' with the given initial state
-- and returns both the final value and the final state.
runStateT :: proxy tag -> StateT tag s m a  -> s -> m (a, s)
runStateT t = Trans.runStateT . untagged t

-- | Runs a 'StateT' with the given initial state
-- and returns the final value, discarding the final state.
evalStateT :: Monad m => proxy tag -> StateT tag s m a  -> s -> m a
evalStateT t = Trans.evalStateT . untagged t

-- | Runs a 'StateT' with the given initial state
-- and returns the final state, discarding the final value.
execStateT :: Monad m => proxy tag -> StateT tag s m a  -> s -> m s
execStateT t = Trans.execStateT . untagged t

-- | Runs a 'State' with the given initial state
-- and returns both the final value and the final state.
runState :: proxy tag -> State tag s a -> s -> (a, s)
runState t = Trans.runState . untagged t

-- | Runs a 'State' with the given initial state
-- and returns the final value, discarding the final state.
evalState :: proxy tag -> State tag s a -> s -> a
evalState t = Trans.evalState . untagged t

-- | Runs a 'State' with the given initial state
-- and returns the final state, discarding the final value.
execState :: proxy tag -> State tag s a -> s -> s
execState t = Trans.execState . untagged t

-- | Transform the computation inside a 'StateT'.
--
-- * @'runStateT' tag ('mapStateT' tag f m) = f . 'runStateT' tag m@
mapStateT :: proxy tag -> (m (a, s) -> n (b, s)) -> StateT tag s m a -> StateT tag s n b
mapStateT t f m = tagged t $ Trans.mapStateT f (coerce m)

-- | Fetch the current value of the state within the monad.
get :: Monad m => proxy tag -> StateT tag s m s
get t = tagged t Trans.get

-- | Set the value of the state within the monad.
put :: Monad m => proxy tag -> s -> StateT tag s m ()
put t = tagged t . Trans.put

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: proxy tag -> Sig.Catch e m (a, s) -> Sig.Catch e (StateT tag s m) a
liftCatch t f m h = tagged t $ Trans.liftCatch f (coerce m) (coerce h)

-- | Lift a @listen@ operation to the new monad.
liftListen :: Monad m => proxy tag -> Sig.Listen w m (a, s) -> Sig.Listen w (StateT tag s m) a
liftListen t listen m = tagged t $ Trans.liftListen listen (coerce m)

-- | In-situ lifting of a @callCC@ operation to the new monad.
-- This version uses the current state on entering the continuation.
-- It does not satisfy the uniformity property (see "Control.Monad.Signatures").
liftCallCC' :: proxy tag -> Sig.CallCC m (a, s) (b, s) -> Sig.CallCC (StateT tag s m) a b
liftCallCC' t callCC f = tagged t $ Trans.liftCallCC' callCC (coerce f)

-- | Lift a @pass@ operation to the new monad.
liftPass :: Monad m => proxy tag -> Sig.Pass w m (a,s) -> Sig.Pass w (StateT tag s m) a
liftPass t pass m = tagged t $ Trans.liftPass pass (coerce m)

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
