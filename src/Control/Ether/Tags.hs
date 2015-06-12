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

type family MaybeToList (mt :: Maybe k) :: [k] where
    MaybeToList 'Nothing = '[]
    MaybeToList ('Just t) = '[t]

type family MaybeMapTag (as :: Maybe (* -> *)) :: Maybe * where
    MaybeMapTag 'Nothing  = 'Nothing
    MaybeMapTag ('Just a) = Tag a

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

    -- | The 'Inner' type family equals @Nothing@ for most types, but for
    -- monad transformers with inner monad @m@ it equals @Just m@.
    type Inner m :: Maybe (* -> *)
    type instance Inner m = 'Nothing

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
    type Inner (Trans.ContT r m) = 'Just m

instance Taggable (Trans.ExceptT e m) where
    type Inner (Trans.ExceptT e m) = 'Just m

instance Taggable (Trans.IdentityT m) where
    type Inner (Trans.IdentityT m) = 'Just m

instance Taggable (Trans.ListT m) where
    type Inner (Trans.ListT m) = 'Just m

instance Taggable (Trans.MaybeT m) where
    type Inner (Trans.MaybeT m) = 'Just m

instance Taggable (Trans.ReaderT r m) where
    type Inner (Trans.ReaderT r m) = 'Just m

instance Taggable (Trans.Lazy.StateT s m) where
    type Inner (Trans.Lazy.StateT s m) = 'Just m

instance Taggable (Trans.Strict.StateT s m) where
    type Inner (Trans.Strict.StateT s m) = 'Just m

instance Taggable (Trans.Lazy.WriterT w m) where
    type Inner (Trans.Lazy.WriterT w m) = 'Just m

instance Taggable (Trans.Strict.WriterT w m) where
    type Inner (Trans.Strict.WriterT w m) = 'Just m

type Tags (m :: * -> *) = MaybeToList (Tag m) ++ MaybeToList (MaybeMapTag (Inner m))

-- | The 'Tagged' type class establishes a relationship between a tagged
-- monad transformer and its untagged counterpart.
class (Taggable m, Tag m ~ 'Just tag) => Tagged m tag | m -> tag where
    type Untagged m :: * -> *
    tagged :: proxy tag -> Untagged m a -> m a
    untagged :: proxy tag -> m a -> Untagged m a
