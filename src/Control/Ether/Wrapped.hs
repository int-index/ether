{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Ether.Wrapped where

import Data.Proxy (Proxy(Proxy))
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Ether.Reader

import qualified Control.Monad.Reader as Class

newtype WrappedEther tag m a = WrapEther { unwrapEther :: m a }
    deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
             , MonadFix, MonadIO )

instance MonadEtherReader tag r m => Class.MonadReader r (WrappedEther tag m) where
    ask = WrapEther $ etherAsk (Proxy :: Proxy tag)
    local f = WrapEther . etherLocal (Proxy :: Proxy tag) f . unwrapEther

ethered :: proxy tag -> WrappedEther tag m a -> m a
ethered _proxy = unwrapEther
