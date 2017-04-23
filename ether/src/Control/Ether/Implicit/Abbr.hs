-- | See "Control.Ether.Abbr".

module Control.Ether.Implicit.Abbr (R, W, S, E) where

import Control.Ether.Abbr (ReifyAbbr)
import Control.Monad.Ether.Implicit

-- | Denotes 'MonadReader'.
data R r
type instance ReifyAbbr (R r) = MonadReader r

-- | Denotes 'MonadWriter'.
data W w
type instance ReifyAbbr (W w) = MonadWriter w

-- | Denotes 'MonadState'.
data S s
type instance ReifyAbbr (S s) = MonadState s

-- | Denotes 'MonadExcept'.
data E e
type instance ReifyAbbr (E e) = MonadExcept e

