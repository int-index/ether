{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Annotating monads with tags to turn untagged constraints into tagged ones.

> import qualified Control.Monad.State as T
> import Control.Ether.TH (ethereal)
> import Control.Monad.Ether.State (MonadState)
> import Control.Ether.Wrapped (ethered)
>
> ethereal "Foo" "foo"
>
> f :: T.MonadState Int m => m String
> f = fmap show T.get
> 
> g :: MonadState Foo Int m => m String
> g = ethered foo f
-}

module Control.Ether.Wrapped
    ( WrappedEther(..)
    , ethered
    ) where

import Data.Proxy (Proxy(Proxy))
import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import GHC.Generics (Generic)
import qualified Control.Newtype as NT

import Control.Monad.Ether.Reader.Class
import Control.Monad.Ether.State.Class
import Control.Monad.Ether.Except.Class
import Control.Monad.Ether.Writer.Class

import qualified Control.Monad.Reader as Class
import qualified Control.Monad.State  as Class
import qualified Control.Monad.Except as Class
import qualified Control.Monad.Writer as Class

-- | Wrap a monad to attach a tag to it.
newtype WrappedEther tag m a = WrapEther { unwrapEther :: m a }
    deriving ( Generic
             , Functor, Applicative, Alternative, Monad, MonadPlus
             , MonadFix, MonadIO )

instance NT.Newtype (WrappedEther tag m a)

-- | Annotate a polymorphic monadic computation with a tag.
ethered :: proxy tag -> WrappedEther tag m a -> m a
ethered _proxy = unwrapEther

instance MonadReader tag r m => Class.MonadReader r (WrappedEther tag m) where
    ask = WrapEther $ ask (Proxy :: Proxy tag)
    local f = WrapEther . local (Proxy :: Proxy tag) f . unwrapEther

instance MonadReader tag r m => MonadReader tag r (WrappedEther tag' m) where
    ask t = WrapEther $ ask t
    local t f = WrapEther . local t f . unwrapEther

instance MonadState tag s m => Class.MonadState s (WrappedEther tag m) where
    get = WrapEther $ get (Proxy :: Proxy tag)
    put = WrapEther . put (Proxy :: Proxy tag)

instance MonadState tag s m => MonadState tag s (WrappedEther tag' m) where
    get t = WrapEther $ get t
    put t = WrapEther . put t

instance MonadExcept tag e m => Class.MonadError e (WrappedEther tag m) where
    throwError = WrapEther . throw (Proxy :: Proxy tag)
    catchError m h = WrapEther $ catch (Proxy :: Proxy tag) (unwrapEther m) (unwrapEther . h)

instance MonadExcept tag e m => MonadExcept tag e (WrappedEther tag' m) where
    throw t = WrapEther . throw t
    catch t m h = WrapEther $ catch t (unwrapEther m) (unwrapEther . h)

instance MonadWriter tag w m => Class.MonadWriter w (WrappedEther tag m) where
    tell = WrapEther . tell (Proxy :: Proxy tag)
    listen = WrapEther . listen (Proxy :: Proxy tag) . unwrapEther
    pass = WrapEther . pass (Proxy :: Proxy tag) . unwrapEther

instance MonadWriter tag w m => MonadWriter tag w (WrappedEther tag' m) where
    tell t = WrapEther . tell t
    listen t = WrapEther . listen t . unwrapEther
    pass t = WrapEther . pass t . unwrapEther
