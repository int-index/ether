{-# LANGUAGE TemplateHaskell #-}
module Control.Monad.Ether.Implicit.Except.TH (try', try) where

import qualified Language.Haskell.TH as TH
import Control.Ether.TH (deepN)
import Control.Monad.Ether.Implicit.Except

try' :: Functor m => ExceptT e m a -> (e -> a) -> m a
try' m h = either h id <$> runExceptT m

try :: Int -> TH.ExpQ
try n = deepN n [|try'|]
