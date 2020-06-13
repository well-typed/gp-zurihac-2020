module Section_1_6 where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

data Colour = Red | Green | Blue
  deriving Show

newtype Wrap a = Wrap a
newtype Con (n :: Symbol) a = Con a

class Generic a where
  type Rep a :: Type

  from :: a -> Rep a
  to :: Rep a -> a

instance Generic (Tree a) where
  type Rep (Tree a) =
    Either (Con "Left" (Wrap a))
           (Con "Node" (Wrap (Tree a), Wrap (Tree a)))

  from :: Tree a -> Rep (Tree a)
  from (Leaf x) = Left (Con (Wrap x))
  from (Node l r) = Right (Con (Wrap l, Wrap r))

  to :: Rep (Tree a) -> Tree a
  to (Left (Con (Wrap x))) = Leaf x
  to (Right (Con (Wrap l, Wrap r))) = Node l r

instance Generic Colour where
  type Rep Colour =
    Either    (Con "Red" ())
      (Either (Con "Green" ())
              (Con "Blue" ()))

  from :: Colour -> Rep Colour
  from Red = Left (Con ())
  from Green = Right (Left (Con ()))
  from Blue = Right (Right (Con ()))

  to :: Rep Colour -> Colour
  to (Left (Con ())) = Red
  to (Right (Left (Con ()))) = Green
  to (Right (Right (Con ()))) = Blue

eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
eq a1 a2 = geq (from a1) (from a2)

class GEq a where
  geq :: a -> a -> Bool

instance (GEq a, GEq b) => GEq (Either a b) where
  geq (Left a1) (Left a2) = geq a1 a2
  geq (Right b1) (Right b2) = geq b1 b2
  geq _ _ = False

instance GEq a => GEq (Con n a) where
  geq (Con a1) (Con a2) = geq a1 a2

instance (GEq a, GEq b) => GEq (a, b) where
  geq (a1, b1) (a2, b2) = geq a1 a2 && geq b1 b2

instance GEq () where
  geq () () = True

instance Eq a => GEq (Wrap a) where
  geq (Wrap a1) (Wrap a2) = a1 == a2

instance Eq a => Eq (Tree a) where
  (==) = eq

instance Eq Colour where
  (==) = eq

enum :: (Generic a, GEnum (Rep a)) => [a]
enum = to <$> genum

class GEnum a where
  genum :: [a]

instance GEnum () where
  genum = [()]

instance (GEnum a, GEnum b) => GEnum (Either a b) where
  genum = (Left <$> genum) ++ (Right <$> genum)

instance GEnum a => GEnum (Con n a) where
  genum = Con <$> genum

conName :: (Generic a, GConName (Rep a)) => a -> String
conName a = gconName (from a)

class GConName a where
  gconName :: a -> String

instance KnownSymbol n => GConName (Con n a) where
  gconName _ = symbolVal (Proxy @n)

instance (GConName a, GConName b) => GConName (Either a b) where
  gconName (Left a) = gconName a
  gconName (Right b) = gconName b

conIx :: (Generic a, GConIx (Rep a)) => a -> Int
conIx a = gconIx (from a)

class GConIx a where
  gconIx :: a -> Int
  gconCount :: Int

instance GConIx (Con n a) where
  gconIx _ = 0
  gconCount = 1

instance (GConIx a, GConIx b) => GConIx (Either a b) where
  gconIx (Left a) = gconIx a
  gconIx (Right b) = gconCount @a + gconIx b
  gconCount = gconCount @a + gconCount @b

test :: [Int]
test = conIx <$> enum @Colour
