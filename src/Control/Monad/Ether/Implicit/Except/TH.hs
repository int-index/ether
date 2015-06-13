{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell utilities for exception handling.

module Control.Monad.Ether.Implicit.Except.TH (try', try) where

import qualified Language.Haskell.TH as TH
import Control.Ether.TH (deepN)
import Control.Monad.Ether.Implicit.Except

-- | Basic building block for 'try'. Runs an 'ExceptT' with a handler.
try' :: Functor m => ExceptT e m a -> (e -> a) -> m a
try' m h = fmap (either h id) (runExceptT m)

-- | Handle @n@ exceptions with supplied handlers.
--
-- > $(try 3) monadicComputation
-- >    (\Exception1 -> ...)
-- >    (\Exception2 -> ...)
-- >    (\Exception3 -> ...)
try :: Int -> TH.ExpQ
try n = deepN n [|try'|]
