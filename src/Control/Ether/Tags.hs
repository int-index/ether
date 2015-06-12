{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Ether.Tags
    ( Taggable(..)
    , Tagged(..)
    , Tags
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

type family MaybeToList (mt :: Maybe *) :: [*] where
    MaybeToList 'Nothing = '[]
    MaybeToList ('Just t) = '[t]

type family (as :: [*]) ++ (bs :: [*]) :: [*] where
    '[] ++ bs = bs
    (a ': as) ++ bs = a ': (as ++ bs)

-- | The 'Taggable' class defines the type families to manage tags in monad
-- transformer stacks. Its kind is restricted to @* -> *@ to prevent incorrect
-- instances.
class Taggable (m :: * -> *) where

    -- | The 'Tag' type family equals @Nothing@ for most types, but for tagged
    -- monad transformers it equals @Just tag@.
    type Tag m :: Maybe *
    type instance Tag m = 'Nothing

    -- | The 'Tags'' type family returns a type-level list of the inner monad
    -- of a monad transformer. Instances should be defined by passing the inner
    -- monad to the 'Tags' type function.
    type Tags' m :: [*]
    type instance Tags' m = '[]

instance Taggable IO
instance Taggable Identity

instance Taggable Maybe
instance Taggable Last
instance Taggable First
instance Taggable ((->) r)
instance Taggable STM
instance Taggable (Either e)
instance Taggable Proxy
instance Taggable (Strict.ST s)
instance Taggable (Lazy.ST s)

instance Taggable (Trans.ContT r m) where
    type Tags' (Trans.ContT r m) = Tags m

instance Taggable (Trans.ExceptT e m) where
    type Tags' (Trans.ExceptT e m) = Tags m

instance Taggable (Trans.IdentityT m) where
    type Tags' (Trans.IdentityT m) = Tags m

instance Taggable (Trans.ListT m) where
    type Tags' (Trans.ListT m) = Tags m

instance Taggable (Trans.MaybeT m) where
    type Tags' (Trans.MaybeT m) = Tags m

instance Taggable (Trans.ReaderT r m) where
    type Tags' (Trans.ReaderT r m) = Tags m

instance Taggable (Trans.Lazy.StateT s m) where
    type Tags' (Trans.Lazy.StateT s m) = Tags m

instance Taggable (Trans.Strict.StateT s m) where
    type Tags' (Trans.Strict.StateT s m) = Tags m

instance Taggable (Trans.Lazy.WriterT w m) where
    type Tags' (Trans.Lazy.WriterT w m) = Tags m

instance Taggable (Trans.Strict.WriterT w m) where
    type Tags' (Trans.Strict.WriterT w m) = Tags m

-- | The 'Tags' type function combines the results of 'Tag' and 'Tags''.
type Tags (m :: * -> *) = MaybeToList (Tag m) ++ Tags' m

-- | The 'Tagged' type class establishes a relationship between a tagged
-- monad transformer and its untagged counterpart.
class (Taggable m, Tag m ~ 'Just tag) => Tagged m tag | m -> tag where
    type Untagged m :: * -> *
    tagged :: proxy tag -> Untagged m a -> m a
    untagged :: proxy tag -> m a -> Untagged m a
