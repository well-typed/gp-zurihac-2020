module GP where

import Data.Coerce
import Data.Functor.Const
import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import Data.Void
import GHC.TypeLits

data Tree a = Leaf a | Node (Tree a) (Tree a)
