{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | See "Control.Monad.Trans.Except".

module Control.Monad.Trans.Ether.Except
    (
    -- * The Except monad
      Except
    , except
    , runExcept
    -- * The ExceptT monad transformer
    , ExceptT
    , exceptT
    , runExceptT
    , mapExceptT
    -- * Exception operations
    , throw
    , catch
    -- * Lifting other operations
    , liftCallCC
    , liftListen
    , liftPass
    , liftCatch
    ) where

import Data.Proxy (Proxy(Proxy))
import Data.Functor.Identity (Identity(..))
import Data.Coerce (coerce)
import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO)
import Control.Ether.Tagged (Taggable(..), Tagged(..))
import qualified Control.Ether.Util as Util
import GHC.Generics (Generic)
import qualified Control.Newtype as NT

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
runExcept t = Trans.runExcept . untagged t

-- | The exception monad transformer.
--
-- The 'return' function returns a normal value, while '>>=' exits on
-- the first exception.

newtype ExceptT tag e m a = ExceptT (Trans.ExceptT e m a)
    deriving ( Generic
             , Functor, Applicative, Alternative, Monad, MonadPlus
             , MonadFix, MonadTrans, MonadIO )

instance NT.Newtype (ExceptT tag e m a)

instance Taggable (ExceptT tag e m) where
    type Tag (ExceptT tag e m) = 'Just tag
    type Inner (ExceptT tag e m) = 'Just m

instance Tagged (ExceptT tag e m) tag where
    type Untagged (ExceptT tag e m) = Trans.ExceptT e m

-- | Constructor for computations in the exception monad transformer.
exceptT :: proxy tag -> m (Either e a) -> ExceptT tag e m a
exceptT t = tagged t . Trans.ExceptT

-- | Constructor for computations in the exception monad
-- (the inverse of 'runExcept').
except :: Monad m => proxy tag -> Either e a -> ExceptT tag e m a
except t = exceptT t . return

-- | Runs an 'ExceptT' and returns either an exception or a normal value.
runExceptT :: proxy tag -> ExceptT tag e m a -> m (Either e a)
runExceptT t = Trans.runExceptT . untagged t

-- | Transforms the computation inside an 'ExceptT'.
--
-- * @'runExceptT' tag ('mapExceptT' tag f m) = f ('runExceptT' tag m)@
mapExceptT
    :: proxy tag
    -> (m (Either e a) -> n (Either e' b))
    -> ExceptT tag e  m a
    -> ExceptT tag e' n b
mapExceptT t f m = tagged t $ Trans.mapExceptT f (coerce m)

-- | Is used within a monadic computation to begin exception processing.
throw :: Monad m => proxy tag -> e -> ExceptT tag e m a
throw t = tagged t . Trans.throwE

-- | A handler function to handle previous exceptions and return to normal execution.
catch :: Monad m => proxy tag -> ExceptT tag e m a -> (e -> ExceptT tag e m a) -> ExceptT tag e m a
catch t m h = tagged t $ Trans.catchE (coerce m) (coerce . h)

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: proxy tag -> Sig.CallCC m (Either e a) (Either e b) -> Sig.CallCC (ExceptT tag e m) a b
liftCallCC t callCC f = tagged t $ Trans.liftCallCC callCC (coerce f)

-- | Lift a @listen@ operation to the new monad.
liftListen :: Monad m => proxy tag -> Sig.Listen w m (Either e a) -> Sig.Listen w (ExceptT tag e m) a
liftListen t listen m = tagged t $ Trans.liftListen listen (coerce m)

-- | Lift a @pass@ operation to the new monad.
liftPass :: Monad m => proxy tag -> Sig.Pass w m (Either e a) -> Sig.Pass w (ExceptT tag e m) a
liftPass t pass m = tagged t $ Trans.liftPass pass (coerce m)

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: proxy tag -> Sig.Catch e m (Either e' a) -> Sig.Catch e (ExceptT tag e' m) a
liftCatch t catchE m h = tagged t $ Util.liftCatch_ExceptT catchE (coerce m) (coerce h)

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
