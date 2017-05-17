{- |

Type-level machinery to manipulate constraints on the monad
transformer stack.

Out of the box it provides the following dispatch strategies:

* 'tagAttach' to use functions defined using untagged monad classes
  as if they were defined using tagged ones.

* 'tagReplace' to use functions defined using one tag
  as if they were defined using another one.

> import Ether
> import Control.Monad.State as Mtl
>
> data Foo
> data Bar
>
> f :: Mtl.MonadState Int m => m String
> f = fmap show Mtl.get
>
> g :: Ether.MonadState Foo Int m => m String
> g = tagAttach @Foo f
>
> h :: Ether.MonadState Bar Int m => m String
> h = tagReplace @Foo @Bar g

-}

module Ether.TagDispatch
  (
  -- * The Tag Attach monad transformer
    TagAttachT
  , tagAttach
  -- * The Tag Replace monad transformer
  , TagReplaceT
  , tagReplace
  -- * Internal labels
  , TAG_ATTACH
  , TAG_REPLACE
  ) where

import qualified Control.Monad.Error.Class   as Mtl
import qualified Control.Monad.Reader.Class  as Mtl
import qualified Control.Monad.State.Class   as Mtl
import qualified Control.Monad.Writer.Class  as Mtl

import Ether.Except
import Ether.Reader
import Ether.State
import Ether.Writer
import Ether.TaggedTrans

import Control.Monad.Trans.Identity
import Data.Coerce

-- | Encode type-level information for 'tagAttach'.
data TAG_ATTACH t

type TagAttachT t = TaggedTrans (TAG_ATTACH t) IdentityT

-- | Attach a tag to untagged transformers.
tagAttach :: forall tag m a . TagAttachT tag m a -> m a
tagAttach = coerce (runIdentityT @_ @m @a)

instance {-# OVERLAPPING #-}
    ( MonadReader tag r m, trans ~ IdentityT
    ) => Mtl.MonadReader r (TaggedTrans (TAG_ATTACH tag) trans m)
  where
    ask = ask @tag
    local = local @tag
    reader = reader @tag

instance {-# OVERLAPPING #-}
    ( MonadState tag s m, trans ~ IdentityT
    ) => Mtl.MonadState s (TaggedTrans (TAG_ATTACH tag) trans m)
  where
    get = get @tag
    put = put @tag
    state = state @tag

instance {-# OVERLAPPING #-}
    ( MonadExcept tag e m, trans ~ IdentityT
    ) => Mtl.MonadError e (TaggedTrans (TAG_ATTACH tag) trans m)
  where
    throwError = throw @tag
    catchError = catch @tag

instance {-# OVERLAPPING #-}
    ( MonadWriter tag w m, trans ~ IdentityT
    ) => Mtl.MonadWriter w (TaggedTrans (TAG_ATTACH tag) trans m)
  where
    writer = writer @tag
    tell = tell @tag
    listen = listen @tag
    pass = pass @tag

-- | Encode type-level information for 'tagReplace'.
data TAG_REPLACE tOld tNew

type TagReplaceT tOld tNew = TaggedTrans (TAG_REPLACE tOld tNew) IdentityT

-- | Replace a tag with another tag.
tagReplace :: forall tOld tNew m a . TagReplaceT tOld tNew m a -> m a
tagReplace = coerce (runIdentityT @_ @m @a)

instance
    ( MonadReader tNew r m, trans ~ IdentityT
    ) => MonadReader tOld r (TaggedTrans (TAG_REPLACE tOld tNew) trans m)
  where
    ask = ask @tNew
    local = local @tNew
    reader = reader @tNew

instance
    ( MonadState tNew s m, trans ~ IdentityT
    ) => MonadState tOld s (TaggedTrans (TAG_REPLACE tOld tNew) trans m)
  where
    get = get @tNew
    put = put @tNew
    state = state @tNew

instance
    ( MonadExcept tNew e m, trans ~ IdentityT
    ) => MonadExcept tOld e (TaggedTrans (TAG_REPLACE tOld tNew) trans m)
  where
    throw = throw @tNew
    catch = catch @tNew

instance
    ( MonadWriter tNew w m, trans ~ IdentityT
    ) => MonadWriter tOld w (TaggedTrans (TAG_REPLACE tOld tNew) trans m)
  where
    writer = writer @tNew
    tell = tell @tNew
    listen = listen @tNew
    pass = pass @tNew
