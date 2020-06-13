module Section_1_4 where

import Data.Kind

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

data Colour = Red | Green | Blue
  deriving Show

newtype Wrap a = Wrap a

class Generic a where
  type Rep a :: Type

  from :: a -> Rep a
  to :: Rep a -> a

instance Generic (Tree a) where
  type Rep (Tree a) = Either (Wrap a) (Wrap (Tree a), Wrap (Tree a))

  from :: Tree a -> Rep (Tree a)
  from (Leaf x) = Left (Wrap x)
  from (Node l r) = Right (Wrap l, Wrap r)

  to :: Rep (Tree a) -> Tree a
  to (Left (Wrap x)) = Leaf x
  to (Right (Wrap l, Wrap r)) = Node l r

instance Generic Colour where
  type Rep Colour = Either () (Either () ())

  from :: Colour -> Rep Colour
  from Red = Left ()
  from Green = Right (Left ())
  from Blue = Right (Right ())

  to :: Rep Colour -> Colour
  to (Left ()) = Red
  to (Right (Left ())) = Green
  to (Right (Right ())) = Blue

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

test :: [Colour]
test = enum
