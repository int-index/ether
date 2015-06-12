{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , mapWriterT
    -- * Writer operations
    , tell
    -- * Lifting other operations
    , liftCallCC
    , liftCatch
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
import qualified Control.Ether.Util as Util
import GHC.Generics (Generic)
import qualified Control.Newtype as NT

import qualified Control.Monad.Signatures as Sig
import qualified Control.Monad.Trans.Writer.Lazy as Trans

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
newtype WriterT tag w m a = WriterT (Trans.WriterT w m a)
    deriving ( Generic
             , Functor, Applicative, Alternative, Monad, MonadPlus
             , MonadFix, MonadTrans, MonadIO )

instance NT.Newtype (WriterT tag w m a)

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

-- | Transform the computation inside a 'WriterT'.
--
-- * @'runWriterT' tag ('mapWriterT' tag f m) = f ('runWriterT' tag m)@
mapWriterT :: proxy tag -> (m (a, w) -> n (b, w')) -> WriterT tag w m a -> WriterT tag w' n b
mapWriterT t f m = tagged t $ Trans.mapWriterT f (coerce m)

-- | Appends a value to the accumulator within the monad.
tell :: Monad m => proxy tag -> w -> WriterT tag w m ()
tell t w = writer t ((), w)

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: proxy tag -> Sig.Catch e m (a, w) -> Sig.Catch e (WriterT tag w m) a
liftCatch t f m h = tagged t $ Trans.liftCatch f (coerce m) (coerce h)

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: Monoid w => proxy tag -> Sig.CallCC m (a, w) (b, w) -> Sig.CallCC (WriterT tag w m) a b
liftCallCC t callCC f = tagged t $ Trans.liftCallCC callCC (coerce f)

-- | Lift a @listen@ operation to the new monad.
liftListen :: Monad m => proxy tag -> Sig.Listen w' m (a, w) -> Sig.Listen w' (WriterT tag w m) a
liftListen t listen m = tagged t $ Util.liftListen_WriterT listen (coerce m)

-- | Lift a @pass@ operation to the new monad.
liftPass :: Monad m => proxy tag -> Sig.Pass w' m (a, w) -> Sig.Pass w' (WriterT tag w m) a
liftPass t pass m = tagged t $ Util.liftPass_WriterT pass (coerce m)

instance (Monoid w, Class.MonadCont m) => Class.MonadCont (WriterT tag w m) where
    callCC = liftCallCC Proxy Class.callCC

instance (Monoid w, Class.MonadReader r m) => Class.MonadReader r (WriterT tag w m) where
    ask = lift Class.ask
    local = mapWriterT Proxy . Class.local
    reader = lift . Class.reader

instance (Monoid w, Class.MonadState s m) => Class.MonadState s (WriterT tag w m) where
    get = lift Class.get
    put = lift . Class.put
    state = lift . Class.state

instance (Monoid w, Class.MonadWriter w' m) => Class.MonadWriter w' (WriterT tag w m) where
    writer = lift . Class.writer
    tell   = lift . Class.tell
    listen = liftListen Proxy Class.listen
    pass   = liftPass Proxy Class.pass

instance (Monoid w, Class.MonadError e m) => Class.MonadError e (WriterT tag w m) where
    throwError = lift . Class.throwError
    catchError = liftCatch Proxy Class.catchError
