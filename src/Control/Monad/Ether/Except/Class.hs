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
module Control.Monad.Ether.Except.Class
    ( MonadExcept(..)
    ) where

import Data.Proxy (Proxy(Proxy))
import Control.Monad.Trans (lift)

import Control.Monad.Trans.Ether.Except hiding (throw, catch)
import qualified Control.Monad.Trans.Ether.Except as E
import qualified Control.Monad.Trans.Ether.Reader as R
import qualified Control.Monad.Trans.Ether.Writer as W
import qualified Control.Monad.Trans.Ether.State.Lazy   as S.L
import qualified Control.Monad.Trans.Ether.State.Strict as S.S
import qualified Control.Ether.Util as Util

-- for mtl instances
import qualified Control.Monad.Trans.Except        as Trans.E
import qualified Control.Monad.Trans.Identity      as Trans.I
import qualified Control.Monad.Trans.List          as Trans.L
import qualified Control.Monad.Trans.Maybe         as Trans.M
import qualified Control.Monad.Trans.Reader        as Trans.R
import qualified Control.Monad.Trans.State.Lazy    as Trans.S.Lazy
import qualified Control.Monad.Trans.State.Strict  as Trans.S.Strict
import qualified Control.Monad.Trans.Writer.Lazy   as Trans.W.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Trans.W.Strict

class Monad m => MonadExcept tag e m | m tag -> e where

    -- | Is used within a monadic computation to begin exception processing.
    throw :: proxy tag -> e -> m a

    -- | A handler function to handle previous exceptions and return to
    -- normal execution.
    catch :: proxy tag -> m a -> (e -> m a) -> m a

instance {-# OVERLAPPING #-} Monad m => MonadExcept tag e (ExceptT tag e m) where
    throw = E.throw
    catch = E.catch

instance MonadExcept tag e m => MonadExcept tag e (ExceptT tag' e' m) where
    throw t = lift . throw t
    catch t = liftCatch Proxy (catch t)

-- Instances for other tagged transformers

instance MonadExcept tag e m => MonadExcept tag e (R.ReaderT tag' r m) where
    throw t = lift . throw t
    catch t = R.liftCatch Proxy (catch t)

instance (Monoid w, MonadExcept tag e m) => MonadExcept tag e (W.WriterT tag' w m) where
    throw t = lift . throw t
    catch t = W.liftCatch Proxy (catch t)

instance MonadExcept tag e m => MonadExcept tag e (S.L.StateT tag' s m) where
    throw t = lift . throw t
    catch t = S.L.liftCatch Proxy (catch t)

instance MonadExcept tag e m => MonadExcept tag e (S.S.StateT tag' s m) where
    throw t = lift . throw t
    catch t = S.S.liftCatch Proxy (catch t)


-- Instances for mtl transformers

instance MonadExcept tag e m => MonadExcept tag e (Trans.E.ExceptT e' m) where
    throw t = lift . throw t
    catch t = Util.liftCatch_ExceptT (catch t)

instance MonadExcept tag e m => MonadExcept tag e (Trans.I.IdentityT m) where
    throw t = lift . throw t
    catch t = Trans.I.liftCatch (catch t)

instance MonadExcept tag e m => MonadExcept tag e (Trans.L.ListT m) where
    throw t = lift . throw t
    catch t = Trans.L.liftCatch (catch t)

instance MonadExcept tag e m => MonadExcept tag e (Trans.M.MaybeT m) where
    throw t = lift . throw t
    catch t = Trans.M.liftCatch (catch t)

instance MonadExcept tag e m => MonadExcept tag e (Trans.R.ReaderT r m) where
    throw t = lift . throw t
    catch t = Trans.R.liftCatch (catch t)

instance MonadExcept tag e m => MonadExcept tag e (Trans.S.Lazy.StateT s m) where
    throw t = lift . throw t
    catch t = Trans.S.Lazy.liftCatch (catch t)

instance MonadExcept tag e m => MonadExcept tag e (Trans.S.Strict.StateT s m) where
    throw t = lift . throw t
    catch t = Trans.S.Strict.liftCatch (catch t)

instance (Monoid w, MonadExcept tag e m) => MonadExcept tag e (Trans.W.Lazy.WriterT w m) where
    throw t = lift . throw t
    catch t = Trans.W.Lazy.liftCatch (catch t)

instance (Monoid w, MonadExcept tag e m) => MonadExcept tag e (Trans.W.Strict.WriterT w m) where
    throw t = lift . throw t
    catch t = Trans.W.Strict.liftCatch (catch t)
