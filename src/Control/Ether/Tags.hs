{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Ether.Tags
    ( Tags
    , UniqueTag
    , UniqueTags
    , ensureUniqueTags
    ) where

import Data.Proxy (Proxy)
import Data.Functor.Identity (Identity)
import Data.Monoid (First, Last)
import qualified Control.Monad.ST.Strict as Strict (ST)
import qualified Control.Monad.ST.Lazy   as Lazy   (ST)
import GHC.Conc (STM)
import GHC.Exts (Constraint)

import qualified Control.Monad.Trans.Cont          as Trans
import qualified Control.Monad.Trans.Except        as Trans
import qualified Control.Monad.Trans.Identity      as Trans
import qualified Control.Monad.Trans.List          as Trans
import qualified Control.Monad.Trans.Maybe         as Trans
import qualified Control.Monad.Trans.Reader        as Trans
import qualified Control.Monad.Trans.State.Lazy    as Trans.Lazy
import qualified Control.Monad.Trans.State.Strict  as Trans.Strict
import qualified Control.Monad.Trans.Writer.Lazy   as Trans.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Trans.Strict

-- |
-- The main purpose of the 'UniqueTag' class is to provide clear error
-- messages when the tag uniqueness property is violated. You should never
-- provide instances for it unless you know what you're doing.
class UniqueTag a

type family IsUnique (x :: k) (as :: [k]) :: Constraint where
    IsUnique x (x ': as) = UniqueTag x
    IsUnique x (a ': as) = IsUnique x as
    IsUnique x '[] = ()

type family Unique (as :: [k]) :: Constraint where
    Unique '[] = ()
    Unique (a ': as) = (IsUnique a as, Unique as)

-- |
-- The 'UniqueTags` constraint placed on a type variable representing a
-- monad transformer stack ensures that every tag in the stack appears
-- only once.
type family UniqueTags (m :: * -> *) :: Constraint where
    UniqueTags m = Unique (Tags m)

-- |
-- Type-restricted version of 'id' that adds a 'UniqueTags' constraint to
-- the provided monadic value.
ensureUniqueTags :: UniqueTags m => m a -> m a
ensureUniqueTags = id

-- |
-- The 'Tags' type family returns a type-level list of all tags of its argument
-- @m@. Simple monads have no tags, monad transformers propagate the tags of
-- their inner monads, and tagged monad transformers also add their own tag.
type family Tags (m :: * -> *) :: [*]

type instance Tags IO = '[]
type instance Tags Identity = '[]
type instance Tags [] = '[]
type instance Tags Maybe = '[]
type instance Tags Last = '[]
type instance Tags First = '[]
type instance Tags ((->) r) = '[]
type instance Tags STM = '[]
type instance Tags (Either e) = '[]
type instance Tags Proxy = '[]
type instance Tags (Strict.ST s) = '[]
type instance Tags (Lazy.ST s) = '[]

type instance Tags (Trans.ContT r m) = Tags m
type instance Tags (Trans.ExceptT e m) = Tags m
type instance Tags (Trans.IdentityT m) = Tags m
type instance Tags (Trans.ListT m) = Tags m
type instance Tags (Trans.MaybeT m) = Tags m
type instance Tags (Trans.ReaderT r m) = Tags m
type instance Tags (Trans.Lazy.StateT s m) = Tags m
type instance Tags (Trans.Strict.StateT s m) = Tags m
type instance Tags (Trans.Lazy.WriterT w m) = Tags m
type instance Tags (Trans.Strict.WriterT w m) = Tags m
