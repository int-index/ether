{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Ether.Writer.Class
    ( MonadWriter(..)
    , listens
    , censor
    ) where

import Data.Proxy (Proxy(Proxy))
import Control.Monad.Trans (lift)

import Control.Monad.Trans.Ether.Writer hiding (writer, tell, listen, pass)
import qualified Control.Monad.Trans.Ether.Reader as R
import qualified Control.Monad.Trans.Ether.State.Lazy   as S.L
import qualified Control.Monad.Trans.Ether.State.Strict as S.S
import qualified Control.Monad.Trans.Ether.Except as E
import qualified Control.Monad.Trans.Ether.Writer as W
import qualified Control.Ether.Util as Util

-- for mtl instances
import qualified Control.Monad.Trans.Except        as Trans.E
import qualified Control.Monad.Trans.Identity      as Trans.I
import qualified Control.Monad.Trans.Maybe         as Trans.M
import qualified Control.Monad.Trans.Reader        as Trans.R
import qualified Control.Monad.Trans.State.Lazy    as Trans.S.Lazy
import qualified Control.Monad.Trans.State.Strict  as Trans.S.Strict
import qualified Control.Monad.Trans.Writer.Lazy   as Trans.W.Lazy

class (Monoid w, Monad m) => MonadWriter tag w m | m tag -> w where

    {-# MINIMAL (writer | tell), listen, pass #-}

    -- | Embed a simple writer action.
    writer :: proxy tag -> (a, w) -> m a
    writer t ~(a, w) = do
      tell t w
      return a

    -- | Append a value to the accumulator within the monad.
    tell :: proxy tag -> w -> m ()
    tell t w = writer t ((),w)

    -- | Execute an action and add its accumulator
    -- to the value of the computation.
    listen :: proxy tag -> m a -> m (a, w)

    -- | Execute an action which returns a value and a function,
    -- and return the value, applying the function to the accumulator.
    pass :: proxy tag -> m (a, w -> w) -> m a

listens :: MonadWriter tag w m => proxy tag -> (w -> b) -> m a -> m (a, b)
listens t f m = do
    ~(a, w) <- listen t m
    return (a, f w)

censor :: MonadWriter tag w m => proxy tag -> (w -> w) -> m a -> m a
censor t f m = pass t $ do
    a <- m
    return (a, f)

instance {-# OVERLAPPING #-} (Monoid w, Monad m) => MonadWriter tag w (WriterT tag w m) where
    writer = W.writer
    tell = W.tell
    listen = W.listen
    pass = W.pass

instance (Monoid w', MonadWriter tag w m) => MonadWriter tag w (WriterT tag' w' m) where
    writer t = lift . writer t
    tell   t = lift . tell t
    listen t = W.liftListen Proxy (listen t)
    pass   t = W.liftPass Proxy (pass t)

-- Instances for other tagged transformers

instance (MonadWriter tag w m) => MonadWriter tag w (R.ReaderT tag' r m) where
    writer t = lift . writer t
    tell   t = lift . tell t
    listen t = R.mapReaderT Proxy (listen t)
    pass   t = R.mapReaderT Proxy (pass t)

instance (MonadWriter tag w m) => MonadWriter tag w (E.ExceptT tag' e m) where
    writer t = lift . writer t
    tell   t = lift . tell t
    listen t = E.liftListen Proxy (listen t)
    pass   t = E.liftPass Proxy (pass t)

instance (MonadWriter tag w m) => MonadWriter tag w (S.L.StateT tag' e m) where
    writer t = lift . writer t
    tell   t = lift . tell t
    listen t = S.L.liftListen Proxy (listen t)
    pass   t = S.L.liftPass Proxy (pass t)

instance (MonadWriter tag w m) => MonadWriter tag w (S.S.StateT tag' e m) where
    writer t = lift . writer t
    tell   t = lift . tell t
    listen t = S.S.liftListen Proxy (listen t)
    pass   t = S.S.liftPass Proxy (pass t)

-- Instances for mtl transformers

instance (MonadWriter tag w m) => MonadWriter tag w (Trans.E.ExceptT e m) where
    writer t = lift . writer t
    tell   t = lift . tell t
    listen t = Trans.E.liftListen (listen t)
    pass   t = Trans.E.liftPass (pass t)

instance (MonadWriter tag w m) => MonadWriter tag w (Trans.I.IdentityT m) where
    writer t = lift . writer t
    tell   t = lift . tell t
    listen t = Trans.I.mapIdentityT (listen t)
    pass   t = Trans.I.mapIdentityT (pass t)

instance (MonadWriter tag w m) => MonadWriter tag w (Trans.M.MaybeT m) where
    writer t = lift . writer t
    tell   t = lift . tell t
    listen t = Trans.M.liftListen (listen t)
    pass   t = Trans.M.liftPass (pass t)

instance (MonadWriter tag w m) => MonadWriter tag w (Trans.R.ReaderT r m) where
    writer t = lift . writer t
    tell   t = lift . tell t
    listen t = Trans.R.mapReaderT (listen t)
    pass   t = Trans.R.mapReaderT (pass t)

instance (MonadWriter tag w m) => MonadWriter tag w (Trans.S.Lazy.StateT s m) where
    writer t = lift . writer t
    tell   t = lift . tell t
    listen t = Trans.S.Lazy.liftListen (listen t)
    pass   t = Trans.S.Lazy.liftPass (pass t)

instance (MonadWriter tag w m) => MonadWriter tag w (Trans.S.Strict.StateT s m) where
    writer t = lift . writer t
    tell   t = lift . tell t
    listen t = Trans.S.Strict.liftListen (listen t)
    pass   t = Trans.S.Strict.liftPass (pass t)

instance (Monoid w', MonadWriter tag w m) => MonadWriter tag w (Trans.W.Lazy.WriterT w' m) where
    writer t = lift . writer t
    tell   t = lift . tell t
    listen t = Util.liftListen_WriterT (listen t)
    pass   t = Util.liftPass_WriterT (pass t)
