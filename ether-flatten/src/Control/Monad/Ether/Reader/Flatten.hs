{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}

module Control.Monad.Ether.Reader.Flatten
  ( runReader
  , runReaderT
  ) where

import qualified Control.Monad.Trans.Lift.Local as Lift

import Data.Functor.Identity
import Control.Monad.Ether.Reader.Class as C
import Control.Monad.Trans.Ether.Dispatch
import qualified Control.Monad.Trans.Reader as T
import Control.Lens
import Control.Ether.Flatten

data READER (ts :: [k])

type ReaderT ts r = Dispatch (READER ts) (T.ReaderT r)

type Reader ts r = ReaderT ts r Identity

reflatten
  :: forall tagsOld tagsNew r m a
   . ReaderT tagsOld r m a
  -> ReaderT tagsNew r m a
reflatten = repack
{-# INLINE reflatten #-}

instance
    ( Monad m, Monad (trans m)
    , Lift.MonadTrans trans
    , Lift.LiftLocal trans
    , C.MonadReader tag r m
    ) => C.MonadReader tag r (Dispatch (READER '[]) trans m)
  where
    ask t = Lift.lift (ask t)
    local t = Lift.liftLocal (ask t) (local t)

instance
    ( Monad m, HasLens tag payload r
    , trans ~ T.ReaderT payload
    ) => C.MonadReader tag r (Dispatch (READER (tag ': tags)) trans m)
  where
    ask t = pack $ view (lensOf t)
    {-# INLINE ask #-}
    local t f = pack . T.local (over (lensOf t) f) . unpack
    {-# INLINE local #-}

instance {-# OVERLAPPABLE #-}
    ( Monad m
    , C.MonadReader tag r (Dispatch (READER tags) trans m)
    , trans ~ T.ReaderT payload
    ) => C.MonadReader tag r (Dispatch (READER (t ': tags)) trans m)
  where
    ask t = reflatten @tags @(t ': tags) (C.ask t)
    {-# INLINE ask #-}
    local t f
      = reflatten @tags @(t ': tags)
      . C.local t f
      . reflatten @(t ': tags) @tags
    {-# INLINE local #-}

runReaderT :: ReaderT tags (Product tags as) m a -> Product tags as -> m a
runReaderT m = T.runReaderT (unpack m)

runReader :: Reader tags (Product tags as) a -> Product tags as -> a
runReader m = T.runReader (unpack m)
