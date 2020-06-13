module Section_2_13 where

import Data.Coerce
import Data.Functor.Const
import Data.Functor.Identity
import Data.Kind
import Data.Proxy

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

data Colour = Red | Green | Blue
  deriving Show

type SumRepresentation = [ProductRepresentation]
type ProductRepresentation = [Type]

data Sum (f :: k -> Type) (xs :: [k]) where
  Zero :: f x -> Sum f (x : xs)
  Suc  :: Sum f xs -> Sum f (x : xs)

data Product (f :: k -> Type) (xs :: [k]) where
  Nil  :: Product f '[]
  Cons :: f x -> Product f xs -> Product f (x : xs)

infixr 5 `Cons`

type Interpret xss = Sum (Product Identity) xss

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

type Rep a = Interpret (Code a)
type Representation = SumRepresentation

class Generic a where
  type Code a :: Representation

  from :: a -> Rep a
  to :: Rep a -> a

instance Generic (Tree a) where
  type Code (Tree a) =
    '[ '[ a ], '[ Tree a, Tree a ] ]

  from :: Tree a -> Rep (Tree a)
  from (Leaf x) = Zero (Identity x `Cons` Nil)
  from (Node l r) = Suc (Zero (Identity l `Cons` Identity r `Cons` Nil))

  to :: Rep (Tree a) -> Tree a
  to (Zero (Identity x `Cons` Nil)) = Leaf x
  to (Suc (Zero (Identity l `Cons` Identity r `Cons` Nil))) = Node l r

instance Generic Colour where
  type Code Colour =
    '[ '[], '[], '[] ]

  from :: Colour -> Rep Colour
  from Red = Zero Nil
  from Green = Suc (Zero Nil)
  from Blue = Suc (Suc (Zero Nil))

  to :: Rep Colour -> Colour
  to (Zero Nil) = Red
  to (Suc (Zero Nil)) = Green
  to (Suc (Suc (Zero Nil))) = Blue

zipProduct :: SList c xs -> (forall x . c x => f x -> g x -> h x) -> Product f xs -> Product g xs -> Product h xs
zipProduct SNil _ Nil Nil = Nil
zipProduct (SCons xs) op (fx `Cons` fxs) (gx `Cons` gxs) = op fx gx `Cons` zipProduct xs op fxs gxs

-- This is exercise 17.
mapProduct :: SList c xs -> (forall x . c x => f x -> g x) -> Product f xs -> Product g xs
mapProduct SNil _ Nil = Nil
mapProduct (SCons xs) op (fx `Cons` fxs) = op fx `Cons` mapProduct xs op fxs

collapseProduct :: SList c xs -> Product (Const a) xs -> [a]
collapseProduct SNil Nil = []
collapseProduct (SCons xs) (Const a `Cons` as) = a : collapseProduct xs as

-- This is exercise 18.
collapseSum :: SList c xs -> Sum (Const a) xs -> a
collapseSum (SCons _) (Zero (Const a)) = a
collapseSum (SCons xs) (Suc a) = collapseSum xs a

pureProduct :: SList c xs -> (forall x . c x => f x) -> Product f xs
pureProduct SNil _ = Nil
pureProduct (SCons xs) op = op `Cons` pureProduct xs op

eq :: forall a . (Generic a, IsList (IsList Eq) (Code a)) => a -> a -> Bool
eq a1 a2 = geqSum (list @_ @(IsList Eq) @(Code a)) (from a1) (from a2)

geqSum :: SList (IsList Eq) r -> Sum (Product Identity) r -> Sum (Product Identity) r -> Bool
geqSum xs@(SCons _) (Zero a1) (Zero a2)  = go xs a1 a2
  where
    -- Terrible hack, needed because we cannot more easily get access to the name of the type argument xs.
    -- this would be fixable by allowing explicit pattern matching on types, or by embedding a proxy into
    -- SCons.
    go :: forall xs xss . SList (IsList Eq) (xs : xss) -> Product Identity xs -> Product Identity xs -> Bool
    go (SCons _) = geqProduct (list @_ @Eq @xs)
geqSum (SCons xs) (Suc b1)   (Suc b2)   = geqSum xs b1 b2
geqSum (SCons _)  _          _          = False

geqProduct :: SList Eq r -> Product Identity r -> Product Identity r -> Bool
geqProduct xs a1 a2 =
  and (collapseProduct xs (zipProduct xs (\ a b -> coerce (a == b)) a1 a2))

instance Eq a => Eq (Tree a) where
  (==) = eq

instance Eq Colour where
  (==) = eq

type IsProductType a xs = (Generic a, Code a ~ '[ xs ])

gmempty :: (IsProductType a xs, IsList Monoid xs) => a
gmempty = to (Zero (pureProduct (list @_ @Monoid) (Identity mempty)))

newtype Injection f xs x = Injection (f x -> Sum f xs)

injections :: forall c f xs . SList c xs -> Product (Injection f xs) xs
injections (SCons xs) = Cons (Injection Zero) (mapProduct xs shiftInjection (injections @c @f xs))
injections SNil       = Nil

shiftInjection :: Injection f xs x -> Injection f (y : xs) x
shiftInjection (Injection inj) = Injection (Suc . inj)

applyInjections :: forall c f xs . SList c xs -> Product f xs -> Product (Const (Sum f xs)) xs
applyInjections xs args =
  zipProduct xs (\ (Injection inj) arg -> Const (inj arg))
    (injections @c @f xs) args

enum :: forall a . IsEnumType a => [a]
enum = to <$> genum (list @_ @((~) '[]) @(Code a))

genum :: SList ((~) '[]) xss -> [Sum (Product Identity) xss]
genum xss = collapseProduct xss (applyInjections xss (pureProduct xss Nil))

type IsEnumType a = (Generic a, IsList ((~) '[]) (Code a))

class Generic a => HasDatatypeInfo a where
  conNames :: Proxy a -> Product (Const String) (Code a)

instance HasDatatypeInfo (Tree a) where
  conNames _ = Const "Leaf" `Cons` Const "Node" `Cons` Nil

instance HasDatatypeInfo Colour where
  conNames _ = Const "Red" `Cons` Const "Green" `Cons` Const "Blue" `Cons` Nil

select :: SList c xs -> (forall x . c x => f x -> g x -> h x) -> Product f xs -> Sum g xs -> Sum h xs
select (SCons _) op (Cons fx _) (Zero gx) = Zero (op fx gx)
select (SCons xs) op (Cons _ fxs) (Suc gxs) = Suc (select xs op fxs gxs)

conName :: forall a . (HasDatatypeInfo a, IsList Top (Code a)) => a -> String
conName a = collapseSum xs (select xs const (conNames (Proxy @a)) (from a))
  where
    xs = list @_ @Top
