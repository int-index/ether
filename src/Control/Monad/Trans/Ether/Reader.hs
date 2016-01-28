{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | See "Control.Monad.Trans.Reader".

module Control.Monad.Trans.Ether.Reader
    (
    -- * The Reader monad
      Reader
    , reader
    , runReader
    -- * The ReaderT monad transformer
    , ReaderT
    , readerT
    , runReaderT
    -- * Reader operations
    , ask
    , local
    ) where

import Data.Functor.Identity (Identity(..))
import qualified Control.Monad.Trans.Reader as Trans
import Control.Monad.Trans.Ether.Tagged

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
type ReaderT tag r = TaggedTrans tag (Trans.ReaderT r)

-- | Constructor for computations in the reader monad transformer.
readerT :: proxy tag -> (r -> m a) -> ReaderT tag r m a
readerT _ = pack . Trans.ReaderT

-- | Constructor for computations in the reader monad
-- (the inverse of 'runReader').
reader :: Monad m => proxy tag -> (r -> a) -> ReaderT tag r m a
reader _ = pack . Trans.reader

-- | Runs a 'ReaderT' with the given environment
-- and returns the vinal value.
runReaderT :: proxy tag -> ReaderT tag r m a -> r -> m a
runReaderT _ = Trans.runReaderT . unpack

-- | Runs a 'ReaderT' with the given environment
-- and returns the vinal value.
runReader :: proxy tag -> Reader tag r a -> r -> a
runReader _ = Trans.runReader . unpack

-- | Fetch the value of the environment.
ask :: Monad m => proxy tag -> ReaderT tag r m r
ask _ = pack Trans.ask

-- | Execute a computation in a modified environment
-- (a specialization of 'withReaderT').
--
-- * @'runReaderT' tag ('local' tag f m) = 'runReaderT' tag m . f@
local
    :: proxy tag
    -> (r -> r)
    -- ^ The function to modify the environment.
    -> ReaderT tag r m a
    -- ^ Computation to run in the modified environment.
    -> ReaderT tag r m a
local _ f = pack . Trans.withReaderT f . unpack
