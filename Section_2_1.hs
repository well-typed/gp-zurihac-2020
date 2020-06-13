module Section_2_1 where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

data Colour = Red | Green | Blue
  deriving Show

data Representation =
    Sum Representation Representation
  | Constructor Symbol Representation
  | Product Representation Representation
  | Unit
  | Atom Type

type family Interpret (r :: Representation) :: Type where
  Interpret (Sum r s) = Either (Interpret r) (Interpret s)
  Interpret (Constructor n r) = Interpret r
  Interpret (Product r s) = (Interpret r, Interpret s)
  Interpret Unit = ()
  Interpret (Atom a) = a

type Rep a = Interpret (Code a)

class Generic a where
  type Code a :: Representation

  from :: a -> Rep a
  to :: Rep a -> a

instance Generic (Tree a) where
  type Code (Tree a) =
    Sum (Constructor "Left" (Atom a))
        (Constructor "Node" (Product (Atom (Tree a)) (Atom (Tree a))))

  from :: Tree a -> Rep (Tree a)
  from (Leaf x) = Left x
  from (Node l r) = Right (l, r)

  to :: Rep (Tree a) -> Tree a
  to (Left x) = Leaf x
  to (Right (l, r)) = Node l r

instance Generic Colour where
  type Code Colour =
    Sum       (Constructor "Red" Unit)
      (Sum    (Constructor "Green" Unit)
              (Constructor "Blue" Unit))

  from :: Colour -> Rep Colour
  from Red = Left ()
  from Green = Right (Left ())
  from Blue = Right (Right ())

  to :: Rep Colour -> Colour
  to (Left ()) = Red
  to (Right (Left ())) = Green
  to (Right (Right ())) = Blue

eq :: forall a . (Generic a, GEq (Code a)) => a -> a -> Bool
eq a1 a2 = geq @(Code a) (from a1) (from a2)

class GEq (r :: Representation) where
  geq :: Interpret r -> Interpret r -> Bool

instance (GEq r, GEq s) => GEq (Sum r s) where
  geq (Left a1) (Left a2) = geq @r a1 a2
  geq (Right b1) (Right b2) = geq @s b1 b2
  geq _ _ = False

instance GEq r => GEq (Constructor n r) where
  geq a1 a2 = geq @r a1 a2

instance (GEq r, GEq s) => GEq (Product r s) where
  geq (a1, b1) (a2, b2) = geq @r a1 a2 && geq @s b1 b2

instance GEq Unit where
  geq () () = True

instance Eq a => GEq (Atom a) where
  geq a1 a2 = a1 == a2

instance Eq a => Eq (Tree a) where
  (==) = eq

instance Eq Colour where
  (==) = eq

enum :: forall a . (Generic a, GEnum (Code a)) => [a]
enum = to <$> genum @(Code a)

class GEnum r where
  genum :: [Interpret r]

instance GEnum Unit where
  genum = [()]

instance (GEnum r, GEnum s) => GEnum (Sum r s) where
  genum = (Left <$> genum @r) ++ (Right <$> genum @s)

instance GEnum r => GEnum (Constructor n r) where
  genum = genum @r

conName :: forall a . (Generic a, GConName (Code a)) => a -> String
conName a = gconName @(Code a) (from a)

class GConName r where
  gconName :: Interpret r -> String

instance KnownSymbol n => GConName (Constructor n r) where
  gconName _ = symbolVal (Proxy @n)

instance (GConName r, GConName s) => GConName (Sum r s) where
  gconName (Left a) = gconName @r a
  gconName (Right b) = gconName @s b

conIx :: forall a . (Generic a, GConIx (Code a)) => a -> Int
conIx a = gconIx @(Code a) (from a)

class GConIx r where
  gconIx :: Interpret r -> Int
  gconCount :: Int

instance GConIx (Constructor n r) where
  gconIx _ = 0
  gconCount = 1

instance (GConIx r, GConIx s) => GConIx (Sum r s) where
  gconIx (Left a) = gconIx @r a
  gconIx (Right b) = gconCount @r + gconIx @s b
  gconCount = gconCount @r + gconCount @s

test :: [Int]
test = conIx <$> enum @Colour
