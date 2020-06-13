module Section_2_4 where

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

data SRepresentation (c :: Type -> Constraint) (r :: Representation) where
  SSum :: SRepresentation c r -> SRepresentation c s -> SRepresentation c (Sum r s)
  SConstructor :: KnownSymbol n => Proxy n -> SRepresentation c r -> SRepresentation c (Constructor n r)
  SProduct :: SRepresentation c r -> SRepresentation c s -> SRepresentation c (Product r s)
  SUnit :: SRepresentation c Unit
  SAtom :: c a => SRepresentation c (Atom a)

class IsRepresentation (c :: Type -> Constraint) (r :: Representation) where
  representation :: SRepresentation c r

instance (IsRepresentation c r, IsRepresentation c s) => IsRepresentation c (Sum r s) where
  representation = SSum representation representation

instance (KnownSymbol n, IsRepresentation c r) => IsRepresentation c (Constructor n r) where
  representation = SConstructor Proxy representation

instance (IsRepresentation c r, IsRepresentation c s) => IsRepresentation c (Product r s) where
  representation = SProduct representation representation

instance IsRepresentation c Unit where
  representation = SUnit

instance c a => IsRepresentation c (Atom a) where
  representation = SAtom

class Top a
instance Top a

class (c a, d a) => And c d a
instance (c a, d a) => And c d a

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

eq :: forall a . (Generic a, IsRepresentation Eq (Code a)) => a -> a -> Bool
eq a1 a2 = geq (representation @Eq @(Code a)) (from a1) (from a2)

geq :: SRepresentation Eq r -> Interpret r -> Interpret r -> Bool
geq (SSum r _) (Left a1) (Left a2) = geq r a1 a2
geq (SSum _ s) (Right b1) (Right b2) = geq s b1 b2
geq (SSum _ _) _ _ = False
geq (SConstructor _ r) a1 a2 = geq r a1 a2
geq (SProduct r s) (a1, b1) (a2, b2) = geq r a1 a2 && geq s b1 b2
geq SUnit () () = True
geq SAtom a1 a2 = a1 == a2

instance Eq a => Eq (Tree a) where
  (==) = eq

instance Eq Colour where
  (==) = eq


enum :: forall a . (IsEnumType a, IsRepresentation Top (Code a)) => [a]
enum = to <$> genum (representation @Top @(Code a))

genum :: IsEnumRepresentation r => SRepresentation Top r -> [Interpret r]
genum (SSum r s) = (Left <$> genum r) ++ (Right <$> genum s)
genum (SConstructor _ _) = [()]

type family IsEnumRepresentation (r :: Representation) :: Constraint where
  IsEnumRepresentation (Sum r s) = (IsEnumRepresentation r, IsEnumRepresentation s)
  IsEnumRepresentation (Constructor n r) = r ~ Unit

type IsEnumType a = (Generic a, IsEnumRepresentation (Code a))

