module GP where

import Data.Coerce
import Data.Functor.Const
import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import Data.Void
import GHC.TypeLits

data Tree a = Leaf a | Node (Tree a) (Tree a)

class Generic a where
  type Rep a :: Type

  from :: a -> Rep a
  to :: Rep a -> a

newtype Wrap a = Wrap a
newtype Con (n :: Symbol) a = Con a

instance Generic (Tree a) where
  type Rep (Tree a) =
    Either (Con "Leaf" (Wrap a))
           (Con "Node" (Wrap (Tree a), Wrap (Tree a)))

  from :: Tree a -> Rep (Tree a)
  from (Leaf x) = Left (Con (Wrap x))
  from (Node l r) = Right (Con (Wrap l, Wrap r))

  to :: Rep (Tree a) -> Tree a
  to (Left (Con (Wrap x))) = Leaf x
  to (Right (Con (Wrap l, Wrap r))) = Node l r

eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
eq a1 a2 = geq (from a1) (from a2)

class GEq a where
  geq :: a -> a -> Bool

instance (GEq a, GEq b) => GEq (Either a b) where
  geq (Left a1) (Left a2) = geq a1 a2
  geq (Right b1) (Right b2) = geq b1 b2
  geq _ _ = False

instance (GEq a, GEq b) => GEq (a, b) where
  geq (a1, b1) (a2, b2) = geq a1 a2 && geq b1 b2

instance Eq a => GEq (Wrap a) where
  geq (Wrap a1) (Wrap a2) = a1 == a2

instance GEq () where
  geq () () = True

instance GEq a => GEq (Con n a) where
  geq (Con a1) (Con a2) = geq a1 a2

instance Eq a => Eq (Tree a) where
  (==) = eq

data Colour = Red | Green | Blue
  deriving Show

instance Generic Colour where
  type Rep Colour = Either (Con "Red" ()) (Either (Con "Green" ()) (Con "Blue" ()))

  from :: Colour -> Rep Colour
  from Red = Left (Con ())
  from Green = Right (Left (Con ()))
  from Blue = Right (Right (Con ()))

  to :: Rep Colour -> Colour
  to (Left (Con ())) = Red
  to (Right (Left (Con ()))) = Green
  to (Right (Right (Con ()))) = Blue

instance Eq Colour where
  (==) = eq

enum :: (Generic a, GEnum (Rep a)) => [a]
enum = to <$> genum

class GEnum a where
  genum :: [a]

instance GEnum () where
  genum = [()]

instance GEnum a => GEnum (Con n a) where
  genum = Con <$> genum

instance (GEnum a, GEnum b) => GEnum (Either a b) where
  genum = (Left <$> genum) ++ (Right <$> genum)

conName :: (Generic a, GConName (Rep a)) => a -> String
conName a = gconName (from a)

class GConName a where
  gconName :: a -> String

instance KnownSymbol n => GConName (Con n a) where
  gconName _ = symbolVal (Proxy @n)

instance (GConName a, GConName b) => GConName (Either a b) where
  gconName (Left a) = gconName a
  gconName (Right b) = gconName b

