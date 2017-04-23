-- | This module provides convenience exports of all
-- implicitly tagged monad classes from Ether.

module Control.Monad.Ether.Implicit (module X) where

import Control.Monad as X
import Control.Monad.Fix as X
import Control.Monad.Ether.Implicit.Reader as X
import Control.Monad.Ether.Implicit.Writer as X
import Control.Monad.Ether.Implicit.State as X
import Control.Monad.Ether.Implicit.Except as X
import Control.Monad.Ether.TagAttach as X
import Control.Monad.Ether.TagReplace as X
