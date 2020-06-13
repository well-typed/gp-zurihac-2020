module Section_2_5 where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

data Colour = Red | Green | Blue
  deriving Show

data SumRepresentation =
    Sum SumRepresentation SumRepresentation
  | Constructor Symbol ProductRepresentation

data ProductRepresentation =
    Product ProductRepresentation ProductRepresentation
  | Unit
  | Atom Type

type family InterpretSum (r :: SumRepresentation) :: Type where
  InterpretSum (Sum r s) = Either (InterpretSum r) (InterpretSum s)
  InterpretSum (Constructor n r) = InterpretProduct r

type family InterpretProduct (r :: ProductRepresentation) :: Type where
  InterpretProduct (Product r s) = (InterpretProduct r, InterpretProduct s)
  InterpretProduct Unit = ()
  InterpretProduct (Atom a) = a

data SSumRepresentation (c :: Type -> Constraint) (r :: SumRepresentation) where
  SSum :: SSumRepresentation c r -> SSumRepresentation c s -> SSumRepresentation c (Sum r s)
  SConstructor :: KnownSymbol n => Proxy n -> SProductRepresentation c r -> SSumRepresentation c (Constructor n r)

data SProductRepresentation (c :: Type -> Constraint) (r :: ProductRepresentation) where
  SProduct :: SProductRepresentation c r -> SProductRepresentation c s -> SProductRepresentation c (Product r s)
  SUnit :: SProductRepresentation c Unit
  SAtom :: c a => SProductRepresentation c (Atom a)

class IsSumRepresentation (c :: Type -> Constraint) (r :: SumRepresentation) where
  sumRepresentation :: SSumRepresentation c r

class IsProductRepresentation (c :: Type -> Constraint) (r :: ProductRepresentation) where
  productRepresentation :: SProductRepresentation c r

instance (IsSumRepresentation c r, IsSumRepresentation c s) => IsSumRepresentation c (Sum r s) where
  sumRepresentation = SSum sumRepresentation sumRepresentation

instance (KnownSymbol n, IsProductRepresentation c r) => IsSumRepresentation c (Constructor n r) where
  sumRepresentation = SConstructor Proxy productRepresentation

instance (IsProductRepresentation c r, IsProductRepresentation c s) => IsProductRepresentation c (Product r s) where
  productRepresentation = SProduct productRepresentation productRepresentation

instance IsProductRepresentation c Unit where
  productRepresentation = SUnit

instance c a => IsProductRepresentation c (Atom a) where
  productRepresentation = SAtom

class Top a
instance Top a

class (c a, d a) => And c d a
instance (c a, d a) => And c d a

type Rep a = InterpretSum (Code a)
type Representation = SumRepresentation

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

eq :: forall a . (Generic a, IsSumRepresentation Eq (Code a)) => a -> a -> Bool
eq a1 a2 = geqSum (sumRepresentation @Eq @(Code a)) (from a1) (from a2)

geqSum :: SSumRepresentation Eq r -> InterpretSum r -> InterpretSum r -> Bool
geqSum (SSum r _) (Left a1) (Left a2) = geqSum r a1 a2
geqSum (SSum _ s) (Right b1) (Right b2) = geqSum s b1 b2
geqSum (SSum _ _) _ _ = False
geqSum (SConstructor _ r) a1 a2 = geqProduct r a1 a2

geqProduct :: SProductRepresentation Eq r -> InterpretProduct r -> InterpretProduct r -> Bool
geqProduct (SProduct r s) (a1, b1) (a2, b2) = geqProduct r a1 a2 && geqProduct s b1 b2
geqProduct SUnit () () = True
geqProduct SAtom a1 a2 = a1 == a2

instance Eq a => Eq (Tree a) where
  (==) = eq

instance Eq Colour where
  (==) = eq


enum :: forall a . (IsEnumType a, IsSumRepresentation Top (Code a)) => [a]
enum = to <$> genum (sumRepresentation @Top @(Code a))

genum :: IsEnumRepresentation r => SSumRepresentation Top r -> [InterpretSum r]
genum (SSum r s) = (Left <$> genum r) ++ (Right <$> genum s)
genum (SConstructor _ _) = [()]

type family IsEnumRepresentation (r :: Representation) :: Constraint where
  IsEnumRepresentation (Sum r s) = (IsEnumRepresentation r, IsEnumRepresentation s)
  IsEnumRepresentation (Constructor n r) = r ~ Unit

type IsEnumType a = (Generic a, IsEnumRepresentation (Code a))

