module Ether.Reader
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
    -- * The Reader monad (flattened)
    , Readers
    , runReaders
    -- * The ReaderT monad transformer (flattened)
    , ReadersT
    , runReadersT
    -- * MonadReader class (implicit)
    , MonadReader'
    , local'
    , ask'
    , reader'
    , asks'
    -- * The Reader monad (implicit)
    , Reader'
    , runReader'
    -- * The ReaderT monad transformer (implicit)
    , ReaderT'
    , readerT'
    , runReaderT'
    -- * Internal labels
    , TAGGED
    , READER
    , READERS
    ) where

import qualified Control.Monad.Reader as T
import qualified Control.Monad.Trans.Lift.Local as Lift
import Control.Monad.Trans.Lift.Local (Local)
import Data.Coerce
import Data.Functor.Identity
import Data.Kind

import Ether.Handler
import Ether.Internal

class Monad m => MonadReader tag r m | m tag -> r where

    {-# MINIMAL (ask | reader), local #-}

    -- | Retrieves the monad environment.
    ask :: m r
    ask = reader @tag id

    -- | Executes a computation in a modified environment.
    local
        :: (r -> r)
        -- ^ The function to modify the environment.
        -> m a
        -- ^ @Reader@ to run in the modified environment.
        -> m a

    -- | Retrieves a function of the current environment.
    reader
        :: (r -> a)
        -- ^ The selector function to apply to the environment.
        -> m a
    reader f = fmap f (ask @tag)

instance {-# OVERLAPPABLE #-}
    ( Lift.LiftLocal t
    , Monad (t m)
    , MonadReader tag r m
    ) => MonadReader tag r (t m)
  where

    ask = Lift.lift (ask @tag)
    {-# INLINE ask #-}

    local = Lift.liftLocal (ask @tag) (local @tag)
    {-# INLINE local #-}

    reader = Lift.lift . reader @tag
    {-# INLINE reader #-}

instance {-# OVERLAPPABLE #-}
    ( Monad (trans m)
    , MonadReader tag r (Handler effs trans m)
    ) => MonadReader tag r (Handler (eff ': effs) trans (m :: Type -> Type))
  where

    ask =
      (coerce ::
        Handler         effs  trans m r ->
        Handler (eff ': effs) trans m r)
      (ask @tag)
    {-# INLINE ask #-}

    local =
      (coerce :: forall a .
        Lift.Local r (Handler         effs  trans m) a ->
        Lift.Local r (Handler (eff ': effs) trans m) a)
      (local @tag)
    {-# INLINE local #-}

    reader =
      (coerce :: forall a .
        ((r -> a) -> Handler         effs  trans m a) ->
        ((r -> a) -> Handler (eff ': effs) trans m a))
      (reader @tag)
    {-# INLINE reader #-}

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
        forall eff a . Local r (Handler eff trans m) a
    {-# INLINE local #-}

    reader =
      handling @READER @r @trans @m $
      coerce (T.reader @r @(trans m) @a) ::
        forall eff a . (r -> a) -> Handler eff trans m a
    {-# INLINE reader #-}

instance
    ( HasLens tag payload r
    , Handle READER payload trans
    , Monad m, Monad (trans m)
    ) => MonadReader tag r (Handler (TAGGED READER tag ': effs) trans m)
  where

    ask =
      handling @READER @payload @trans @m $
      (coerce :: forall eff a .
                    trans m a ->
        Handler eff trans m a)
      (T.asks (view (lensOf @tag @payload @r)))
    {-# INLINE ask #-}

    local f =
      handling @READER @payload @trans @m $
      (coerce :: forall eff a .
                    (trans m a ->            trans m a) ->
        (Handler eff trans m a -> Handler eff trans m a))
      (T.local (over (lensOf @tag @payload @r) f))
    {-# INLINE local #-}

type family READERS (ts :: HList xs) :: [Type] where
  READERS 'HNil = '[]
  READERS ('HCons t ts) = TAGGED READER t ': READERS ts

type ReadersT r = Handler (READERS (Tags r)) (T.ReaderT r)

type Readers r = ReadersT r Identity

runReadersT :: forall p m a . ReadersT p m a -> p -> m a
runReadersT = coerce (T.runReaderT @p @_ @m @a)
{-# INLINE runReadersT #-}

runReaders :: forall p a . Readers p a -> p -> a
runReaders = coerce (T.runReader @p @a)
{-# INLINE runReaders #-}

type ReaderT' r = ReaderT r r

readerT' :: (r -> m a) -> ReaderT' r m a
readerT' = readerT
{-# INLINE readerT' #-}

runReaderT' :: ReaderT' r m a -> r -> m a
runReaderT' = runReaderT
{-# INLINE runReaderT' #-}

type Reader' r = Reader r r

runReader' :: Reader' r a -> r -> a
runReader' = runReader
{-# INLINE runReader' #-}

type MonadReader' r = MonadReader r r

local' :: forall r m a . MonadReader' r m => (r -> r) -> m a -> m a
local' = local @r
{-# INLINE local' #-}

ask' :: forall r m . MonadReader' r m => m r
ask' = ask @r
{-# INLINE ask' #-}

reader' :: forall r m a . MonadReader' r m => (r -> a) -> m a
reader' = reader @r
{-# INLINE reader' #-}

asks' :: forall r m a . MonadReader' r m => (r -> a) -> m a
asks' = asks @r
{-# INLINE asks' #-}
