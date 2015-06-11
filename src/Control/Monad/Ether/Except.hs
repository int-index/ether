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
module Control.Monad.Ether.Except
    (
    -- * MonadExcept class
      MonadExcept(..)
    -- * The Except monad
    , Except
    , runExcept
    -- * The ExceptT monad transformer
    , ExceptT
    , exceptT
    , runExceptT
    , mapExceptT
    ) where

import Data.Proxy (Proxy(Proxy))
import Control.Monad.Trans (lift)

import qualified Control.Monad.Trans.Ether.Reader as Reader
import qualified Control.Monad.Trans.Ether.State  as State
import Control.Monad.Trans.Ether.Except

-- for mtl instances
import qualified Control.Monad.Trans.Except        as Trans (ExceptT(..), runExceptT)
import qualified Control.Monad.Trans.Identity      as Trans.I
import qualified Control.Monad.Trans.List          as Trans.L
import qualified Control.Monad.Trans.Maybe         as Trans.M
import qualified Control.Monad.Trans.Reader        as Trans.R
import qualified Control.Monad.Trans.State.Lazy    as Trans.S.Lazy
import qualified Control.Monad.Trans.State.Strict  as Trans.S.Strict
import qualified Control.Monad.Trans.Writer.Lazy   as Trans.W.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Trans.W.Strict

class Monad m => MonadExcept tag e m | m tag -> e where

    throw :: proxy tag -> e -> m a

    catch :: proxy tag -> m a -> (e -> m a) -> m a

instance {-# OVERLAPPING #-} Monad m => MonadExcept tag e (ExceptT tag e m) where
    throw proxy = exceptT proxy . return . Left
    catch proxy m h
        = exceptT proxy
        $ runExceptT proxy m
      >>= either (runExceptT proxy . h) (return . Right)

instance MonadExcept tag e m => MonadExcept tag e (ExceptT tag' e' m) where
    throw proxy = lift . throw proxy
    catch proxy = liftCatch Proxy (catch proxy)

-- Instances for other tagged transformers

instance MonadExcept tag e m => MonadExcept tag e (Reader.ReaderT tag' r m) where
    throw proxy = lift . throw proxy
    catch proxy = Reader.liftCatch Proxy (catch proxy)

instance MonadExcept tag e m => MonadExcept tag e (State.StateT tag' s m) where
    throw proxy = lift . throw proxy
    catch proxy = State.liftCatch Proxy (catch proxy)


-- Instances for mtl transformers

instance MonadExcept tag e m => MonadExcept tag e (Trans.ExceptT e' m) where
    throw proxy = lift . throw proxy
    catch proxy m h = Trans.ExceptT $ catch proxy (Trans.runExceptT m) (Trans.runExceptT . h)

instance MonadExcept tag e m => MonadExcept tag e (Trans.I.IdentityT m) where
    throw proxy = lift . throw proxy
    catch proxy = Trans.I.liftCatch (catch proxy)

instance MonadExcept tag e m => MonadExcept tag e (Trans.L.ListT m) where
    throw proxy = lift . throw proxy
    catch proxy = Trans.L.liftCatch (catch proxy)

instance MonadExcept tag e m => MonadExcept tag e (Trans.M.MaybeT m) where
    throw proxy = lift . throw proxy
    catch proxy = Trans.M.liftCatch (catch proxy)

instance MonadExcept tag e m => MonadExcept tag e (Trans.R.ReaderT r m) where
    throw proxy = lift . throw proxy
    catch proxy = Trans.R.liftCatch (catch proxy)

instance MonadExcept tag e m => MonadExcept tag e (Trans.S.Lazy.StateT s m) where
    throw proxy = lift . throw proxy
    catch proxy = Trans.S.Lazy.liftCatch (catch proxy)

instance MonadExcept tag e m => MonadExcept tag e (Trans.S.Strict.StateT s m) where
    throw proxy = lift . throw proxy
    catch proxy = Trans.S.Strict.liftCatch (catch proxy)

instance (Monoid w, MonadExcept tag e m) => MonadExcept tag e (Trans.W.Lazy.WriterT w m) where
    throw proxy = lift . throw proxy
    catch proxy = Trans.W.Lazy.liftCatch (catch proxy)

instance (Monoid w, MonadExcept tag e m) => MonadExcept tag e (Trans.W.Strict.WriterT w m) where
    throw proxy = lift . throw proxy
    catch proxy = Trans.W.Strict.liftCatch (catch proxy)
