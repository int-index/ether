{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Trans.Ether.Except
    (
    -- * The Except monad
      Except
    , runExcept
    -- * The ExceptT monad transformer
    , ExceptT
    , exceptT
    , runExceptT
    , mapExceptT
    -- * Lifting other operations
    , liftCallCC
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
import qualified Control.Monad.Trans.Except as Trans

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
runExcept proxy m = runIdentity (runExceptT proxy m)

-- | The exception monad transformer.
--
-- The 'return' function returns a normal value, while '>>=' exits on
-- the first exception.

newtype ExceptT tag e m a = ExceptT (Trans.ExceptT e m a)
    deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
             , MonadFix, MonadTrans, MonadIO )

type instance Tags (ExceptT tag r m) = tag ': Tags m

-- | Constructor for computations in the exception monad transformer.
exceptT :: proxy tag -> m (Either e a) -> ExceptT tag e m a
exceptT _proxy = ExceptT . Trans.ExceptT

-- | Runs an 'ExceptT' and returns either an exception or a normal value.
runExceptT :: proxy tag -> ExceptT tag e m a -> m (Either e a)
runExceptT _proxy (ExceptT (Trans.ExceptT f)) = f

-- | Transform the computation inside an 'ExceptT'.
--
-- * @'runExceptT' tag ('mapExceptT' tag f m) = f ('runExceptT' tag m)@
mapExceptT
    :: proxy tag
    -> (m (Either e a) -> n (Either e' b))
    -> ExceptT tag e  m a
    -> ExceptT tag e' n b
mapExceptT _proxy f m = ExceptT $ Trans.mapExceptT f (coerce m)

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: proxy tag -> Sig.CallCC m (Either e a) (Either e b) -> Sig.CallCC (ExceptT tag e m) a b
liftCallCC _proxy callCC f = ExceptT $ Trans.liftCallCC callCC (coerce f)

-- | Lift a @listen@ operation to the new monad.
liftListen :: Monad m => proxy tag -> Sig.Listen w m (Either e a) -> Sig.Listen w (ExceptT tag e m) a
liftListen _proxy listen m = ExceptT $ Trans.liftListen listen (coerce m)

-- | Lift a @pass@ operation to the new monad.
liftPass :: Monad m => proxy tag -> Sig.Pass w m (Either e a) -> Sig.Pass w (ExceptT tag e m) a
liftPass _proxy pass m = ExceptT $ Trans.liftPass pass (coerce m)

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: proxy tag -> Sig.Catch e m (Either e' a) -> Sig.Catch e (ExceptT tag e' m) a
liftCatch proxy catchE m h = exceptT proxy $ catchE (runExceptT proxy m) (runExceptT proxy . h)

instance Class.MonadCont m => Class.MonadCont (ExceptT tag e m) where
    callCC = liftCallCC Proxy Class.callCC

instance Class.MonadReader r m => Class.MonadReader r (ExceptT tag e m) where
    ask = lift Class.ask
    local = mapExceptT Proxy . Class.local
    reader = lift . Class.reader

instance Class.MonadState s m => Class.MonadState s (ExceptT tag e m) where
    get = lift Class.get
    put = lift . Class.put
    state = lift . Class.state

instance Class.MonadWriter w m => Class.MonadWriter w (ExceptT tag e m) where
    writer = lift . Class.writer
    tell   = lift . Class.tell
    listen = liftListen Proxy Class.listen
    pass   = liftPass Proxy Class.pass

instance Class.MonadError e' m => Class.MonadError e' (ExceptT tag e m) where
    throwError = lift . Class.throwError
    catchError = liftCatch Proxy Class.catchError
