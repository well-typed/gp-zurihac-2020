module Section_2_8 where

import Data.Kind
import Data.Void

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

data Colour = Red | Green | Blue
  deriving Show

type SumRepresentation = [ProductRepresentation]
type ProductRepresentation = [Type]

type family InterpretSum (r :: SumRepresentation) :: Type where
  InterpretSum (r : s) = Either (InterpretProduct r) (InterpretSum s)
  InterpretSum '[]     = Void

type family InterpretProduct (r :: ProductRepresentation) :: Type where
  InterpretProduct (a : s) = (a, InterpretProduct s)
  InterpretProduct '[]     = ()

data SList (c :: k -> Constraint) (xs :: [k]) where
  SCons :: c x => SList c xs -> SList c (x : xs)
  SNil  :: SList c '[]

class IsList (c :: k -> Constraint) (xs :: [k]) where
  list :: SList c xs

instance (c x, IsList c xs) => IsList c (x : xs) where
  list = SCons list

instance IsList c '[] where
  list = SNil

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
    '[ '[ a ], '[ Tree a, Tree a ] ]

  from :: Tree a -> Rep (Tree a)
  from (Leaf x) = Left (x, ())
  from (Node l r) = Right (Left (l, (r, ())))

  to :: Rep (Tree a) -> Tree a
  to (Left (x, ())) = Leaf x
  to (Right (Left (l, (r, ())))) = Node l r

instance Generic Colour where
  type Code Colour =
    '[ '[], '[], '[] ]

  from :: Colour -> Rep Colour
  from Red = Left ()
  from Green = Right (Left ())
  from Blue = Right (Right (Left ()))

  to :: Rep Colour -> Colour
  to (Left ()) = Red
  to (Right (Left ())) = Green
  to (Right (Right (Left ()))) = Blue

eq :: forall a . (Generic a, IsList (IsList Eq) (Code a)) => a -> a -> Bool
eq a1 a2 = geqSum (list @_ @(IsList Eq) @(Code a)) (from a1) (from a2)

geqSum :: SList (IsList Eq) r -> InterpretSum r -> InterpretSum r -> Bool
geqSum xs@(SCons _) (Left a1) (Left a2)  = go xs a1 a2
  where
    -- Terrible hack, needed because we cannot more easily get access to the name of the type argument xs.
    -- this would be fixable by allowing explicit pattern matching on types, or by embedding a proxy into
    -- SCons.
    go :: forall xs xss . SList (IsList Eq) (xs : xss) -> InterpretProduct xs -> InterpretProduct xs -> Bool
    go (SCons _) = geqProduct (list @_ @Eq @xs)
geqSum (SCons xs) (Right b1) (Right b2) = geqSum xs b1 b2
geqSum (SCons _)  _          _          = False

geqProduct :: SList Eq r -> InterpretProduct r -> InterpretProduct r -> Bool
geqProduct (SCons xs) (a1, r1) (a2, r2) = a1 == a2 && geqProduct xs r1 r2
geqProduct SNil       ()       ()       = True

instance Eq a => Eq (Tree a) where
  (==) = eq

instance Eq Colour where
  (==) = eq

-- The following is Exercise 15.

enum :: forall a . (IsEnumType a, IsList Top (Code a)) => [a]
enum = to <$> genum (list @_ @Top @(Code a))

genum :: IsEnumRepresentation r => SList Top r -> [InterpretSum r]
genum (SCons xs) = Left () : (Right <$> genum xs)
genum SNil       = []

type family IsEnumRepresentation (r :: Representation) :: Constraint where
  IsEnumRepresentation (r : s) = (r ~ '[], IsEnumRepresentation s)
  IsEnumRepresentation '[]     = ()

type IsEnumType a = (Generic a, IsEnumRepresentation (Code a))

