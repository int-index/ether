{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Ether.Reader.Flatten
  ( runReader
  , runReaderT
  ) where

import Data.Functor.Identity
import Control.Monad.Ether.Reader.Class as C
import Control.Monad.Trans.Ether.Handler
import qualified Control.Monad.Reader as T
import Control.Lens
import Control.Ether.Flatten
import Data.Coerce

data READER

type family READERS ts where
  READERS '[] = '[]
  READERS (t ': ts) = '(READER, t) ': READERS ts

type ReaderT ts r = Handler (READERS ts) (T.ReaderT r)

type Reader ts r = ReaderT ts r Identity

instance
    ( HasLens tag payload r
    , T.MonadReader payload (trans m) -- FIXME: (forall m . T.MonadReader payload (trans m))
    ) => C.MonadReader tag r (Handler ('(READER, tag) ': dps) trans (m :: * -> *))
  where

    ask =
      (coerce :: forall dp a .
                   trans m a ->
        Handler dp trans m a)
      (view (lensOf @tag))
    {-# INLINE ask #-}

    local f =
      (coerce :: forall dp a .
                   (trans m a ->            trans m a) ->
        (Handler dp trans m a -> Handler dp trans m a))
      (T.local (lensOf @tag %~ f))
    {-# INLINE local #-}

runReaderT
  :: ReaderT tags (Product tags as) m a
  -> Product tags as
  -> m a
runReaderT m = T.runReaderT (coerce m)

runReader
  :: Reader tags (Product tags as) a
  -> Product tags as
  -> a
runReader m = T.runReader (coerce m)
