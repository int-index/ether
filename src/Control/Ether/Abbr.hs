{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Abbreviations for constraints.

module Control.Ether.Abbr
    ( type (-->)
    , type (<--)
    , type (<->)
    , type (-!-)
    , Ether
    , ReifyAbbr
    ) where

import GHC.Exts (Constraint)

import Control.Monad.Ether

-- | Turns an abbreviation into an actual constraint.
type family ReifyAbbr (abbr :: *) (m :: * -> *) :: Constraint

-- | Denotes 'MonadReader'. The mnemonic is that you read values of type @r@
-- from the reader environment tagged by @tag@, thus the arrows points
-- from @tag@ to @r@.
data tag --> r
type instance ReifyAbbr (tag --> r) m = MonadReader tag r m

-- | Denotes 'MonadWriter'. The mnemonic is that you write values of @w@
-- to the writer accumulator tagged by @tag@, thus the arrows points
-- from @w@ to @tag@.
data tag <-- w
type instance ReifyAbbr (tag <-- w) m = MonadWriter tag w m

-- | Denotes 'MonadState'. The mnemonic is that you can both read from and
-- write into the state, thus the arrow points in both directions.
data tag <-> s
type instance ReifyAbbr (tag <-> s) m = MonadState  tag s m

-- | Denotes 'MonadExcept'.
data tag -!- e
type instance ReifyAbbr (tag -!- e) m = MonadExcept tag e m

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

type family Ether (abbrs :: [*]) m :: Constraint where
    Ether '[] m = ()
    Ether (abbr ': abbrs) m = (ReifyAbbr abbr m, Ether abbrs m)
