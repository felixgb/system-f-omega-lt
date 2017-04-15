module Lifetime where

import Syntax
import Control.Monad.Reader
-- instance Ord Lifetime

lifetimeTerm :: Term -> Type Lifetime
lifetimeTerm term = term 
