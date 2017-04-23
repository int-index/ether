-- | Abbreviations for constraints.

module Control.Ether.Abbr
    ( type (-->)
    , type (<--)
    , type (<->)
    , type (-!-)
    , Ether
    , ReifyAbbr
    ) where

import Data.Kind as K
import GHC.Exts (Constraint)

import Control.Monad.Ether

-- | Turns an abbreviation into an actual constraint.
type family ReifyAbbr (abbr :: k) :: (K.Type -> K.Type) -> Constraint

-- | Denotes 'MonadReader'. The mnemonic is that you read values of type @r@
-- from the reader environment tagged by @tag@, thus the arrows points
-- from @tag@ to @r@.
data tag --> r
type instance ReifyAbbr (tag --> r) = MonadReader tag r

-- | Denotes 'MonadWriter'. The mnemonic is that you write values of type @w@
-- to the writer accumulator tagged by @tag@, thus the arrows points
-- from @w@ to @tag@.
data tag <-- w
type instance ReifyAbbr (tag <-- w) = MonadWriter tag w

-- | Denotes 'MonadState'. The mnemonic is that you can both read from and
-- write into the state, thus the arrow points in both directions.
data tag <-> s
type instance ReifyAbbr (tag <-> s) = MonadState  tag s

-- | Denotes 'MonadExcept'.
data tag -!- e
type instance ReifyAbbr (tag -!- e) = MonadExcept tag e

-- | Reify a list of constraint abbreviations.
--
-- > f :: Ether '[Foo --> r, Bar <-- w, Baz <-> s, Quux -!- e] m => m a
--
-- expands into
--
-- > f :: ( MonadReader Foo  r m
-- >      , MonadWriter Bar  w m
-- >      , MonadState  Baz  s m
-- >      , MonadExcept Quux e m
-- >      ) => m a

class U a
instance U a

class (c a, d a) => (c &&& d) a
instance (c a, d a) => (c &&& d) a

type family Ether (abbrs :: [K.Type]) :: (K.Type -> K.Type) -> Constraint where
    Ether '[] = U
    Ether (abbr ': abbrs) = ReifyAbbr abbr &&& Ether abbrs
