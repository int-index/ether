{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | See "Control.Monad.Reader".

module Control.Monad.Ether.Reader
    (
    -- * MonadReader class
      MonadReader
    , ask
    , local
    , reader
    , asks
    -- * The Reader monad
    , Reader
    , runReader
    -- * The ReaderT monad transformer
    , ReaderT
    , readerT
    , runReaderT
    -- *
    , READER
    ) where

import Control.Monad.Ether.Handle
import Control.Monad.Ether.Reader.Class
import Control.Ether.Optic
import Control.Monad.Trans.Ether.Handler
import Control.Monad.Trans.Lift.Local (Local)
import qualified Control.Monad.Reader as T
import Data.Functor.Identity
import Data.Coerce

-- | Retrieves a function of the current environment.
asks
  :: forall tag r m a
   . MonadReader tag r m
  => (r -> a)
  -- ^ The selector function to apply to the environment.
  -> m a
asks = reader @tag
{-# INLINE asks #-}

-- | Encode type-level information for 'ReaderT'.
data READER

-- | The parameterizable reader monad.
--
-- Computations are functions of a shared environment.
--
-- The 'return' function ignores the environment, while '>>=' passes
-- the inherited environment to both subcomputations.
type Reader tag r = ReaderT tag r Identity

-- | The reader monad transformer,
-- which adds a read-only environment to the given monad.
--
-- The 'return' function ignores the environment, while '>>=' passes
-- the inherited environment to both subcomputations.
type ReaderT tag r = Handler (TAGGED READER tag) (T.ReaderT r)

-- | Constructor for computations in the reader monad transformer.
readerT :: forall tag r m a . (r -> m a) -> ReaderT tag r m a
readerT = coerce (T.ReaderT @r @m @a)
{-# INLINE readerT #-}

-- | Runs a 'ReaderT' with the given environment
-- and returns the final value.
runReaderT :: forall tag r m a . ReaderT tag r m a -> r -> m a
runReaderT = coerce (T.runReaderT @r @_ @m @a)
{-# INLINE runReaderT #-}

-- | Runs a 'ReaderT' with the given environment
-- and returns the final value.
runReader :: forall tag r a . Reader tag r a -> r -> a
runReader = coerce (T.runReader @r @a)
{-# INLINE runReader #-}

type instance HandleSuper      READER r trans   = ()
type instance HandleConstraint READER r trans m =
  T.MonadReader r (trans m)

instance Handle READER r (T.ReaderT r) where
  handling r = r
  {-# INLINE handling #-}

instance
    ( Handle READER r trans
    , Monad m, Monad (trans m)
    ) => MonadReader tag r (Handler (TAGGED READER tag) trans m)
  where

    ask =
      handling @READER @r @trans @m $
      coerce (T.ask @r @(trans m))
    {-# INLINE ask #-}

    local =
      handling @READER @r @trans @m $
      coerce (T.local @r @(trans m) @a) ::
        forall dp a . Local r (Handler dp trans m) a
    {-# INLINE local #-}

    reader =
      handling @READER @r @trans @m $
      coerce (T.reader @r @(trans m) @a) ::
        forall dp a . (r -> a) -> Handler dp trans m a
    {-# INLINE reader #-}

instance
    ( HasLens tag payload r
    , Handle READER payload trans
    , Monad m, Monad (trans m)
    ) => MonadReader tag r (Handler (TAGGED READER tag ': dps) trans m)
  where

    ask =
      handling @READER @payload @trans @m $
      (coerce :: forall dp a .
                   trans m a ->
        Handler dp trans m a)
      (T.asks (view (lensOf @tag @payload @r)))
    {-# INLINE ask #-}

    local f =
      handling @READER @payload @trans @m $
      (coerce :: forall dp a .
                   (trans m a ->            trans m a) ->
        (Handler dp trans m a -> Handler dp trans m a))
      (T.local (over (lensOf @tag @payload @r) f))
    {-# INLINE local #-}
