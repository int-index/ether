module Control.Monad.Ether.TagReplace
  ( TAG_REPLACE
  , TagReplaceT
  , tagReplace
  ) where

import Control.Monad.Trans.Identity

import Control.Monad.Ether.Reader.Class
import Control.Monad.Ether.State.Class
import Control.Monad.Ether.Except.Class
import Control.Monad.Ether.Writer.Class

import Control.Monad.Ether.Reader as A
import Control.Monad.Ether.State.Common as A
import Control.Monad.Ether.Except as A
import Control.Monad.Ether.Writer as A

import Control.Monad.Trans.Ether.Handler

import Data.Coerce

-- | Encode type-level information for 'tagReplace'.
data TAG_REPLACE tOld tNew

type TagReplaceT tOld tNew = Handler (TAG_REPLACE tOld tNew) IdentityT

-- | Replace a tag with another tag.
tagReplace :: forall tOld tNew m a . TagReplaceT tOld tNew m a -> m a
tagReplace = coerce (runIdentityT @_ @m @a)
{-# INLINE tagReplace #-}

instance
    ( MonadReader tNew r m, trans ~ IdentityT
    ) => MonadReader tOld r (Handler (TAG_REPLACE tOld tNew) trans m)
  where
    ask = A.ask @tNew
    local = A.local @tNew

instance
    ( MonadState tNew s m, trans ~ IdentityT
    ) => MonadState tOld s (Handler (TAG_REPLACE tOld tNew) trans m)
  where
    get = A.get @tNew
    put = A.put @tNew

instance
    ( MonadExcept tNew e m, trans ~ IdentityT
    ) => MonadExcept tOld e (Handler (TAG_REPLACE tOld tNew) trans m)
  where
    throw = A.throw @tNew
    catch = A.catch @tNew

instance
    ( MonadWriter tNew w m, trans ~ IdentityT
    ) => MonadWriter tOld w (Handler (TAG_REPLACE tOld tNew) trans m)
  where
    writer = A.writer @tNew
    tell = A.tell @tNew
    listen = A.listen @tNew
    pass = A.pass @tNew
