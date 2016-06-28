{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import Control.Monad.Trans.Ether.Dispatch

-- | Encode type-level information for 'tagReplace'.
data TAG_REPLACE tOld tNew

type TagReplaceT tOld tNew = Dispatch (TAG_REPLACE tOld tNew) IdentityT

-- | Replace a tag with another tag.
tagReplace :: forall tOld tNew m a . TagReplaceT tOld tNew m a -> m a
tagReplace = runIdentityT . unpack

instance
    ( MonadReader tNew r m, trans ~ IdentityT
    ) => MonadReader tOld r (Dispatch (TAG_REPLACE tOld tNew) trans m)
  where
    ask   _ = A.ask @tNew
    local _ = A.local @tNew

instance
    ( MonadState tNew s m, trans ~ IdentityT
    ) => MonadState tOld s (Dispatch (TAG_REPLACE tOld tNew) trans m)
  where
    get _ = A.get @tNew
    put _ = A.put @tNew

instance
    ( MonadExcept tNew e m, trans ~ IdentityT
    ) => MonadExcept tOld e (Dispatch (TAG_REPLACE tOld tNew) trans m)
  where
    throw _ = A.throw @tNew
    catch _ = A.catch @tNew

instance
    ( MonadWriter tNew w m, trans ~ IdentityT
    ) => MonadWriter tOld w (Dispatch (TAG_REPLACE tOld tNew) trans m)
  where
    writer _ = A.writer @tNew
    tell   _ = A.tell @tNew
    listen _ = A.listen @tNew
    pass   _ = A.pass @tNew
