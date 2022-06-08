module Test.MySolutions where

import Prelude
import Data.Array (length, nub, nubByEq, nubEq, (:))
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hash, hashEqual)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Newtype (class Newtype, over2, overF, wrap)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show :: Point -> String
  show (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

derive newtype instance eqComplex :: Eq Complex

derive instance newtypeComplex :: Newtype Complex _

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) = show real <> sign <> show imaginary <> "i"
    where
    sign =
      if imaginary >= 0.0 then
        "+"
      else
        ""

instance semiringComplex :: Semiring Complex where
  add = over2 Complex add
  zero = wrap zero
  mul = over2 Complex mul'
    where
    mul' { real: realA, imaginary: imaginaryA } { real: realB, imaginary: imaginaryB } = { real, imaginary }
      where
      real = realA * realB - imaginaryA * imaginaryB

      imaginary = realA * imaginaryB + realB * imaginaryA
  one = wrap { real: one, imaginary: zero }

derive newtype instance ringComplex :: Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

data NonEmpty a
  = NonEmpty a (Array a)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty x xs) = show (x : xs)

derive instance eqNonEmpty :: Eq a => Eq (NonEmpty a)

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty a as) (NonEmpty b bs) = NonEmpty a (append as $ b : bs)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty a as) = NonEmpty (f a) (map f as)

data Extended a
  = Infinite
  | Finite a

derive instance eqExtended :: Eq a => Eq (Extended a)

instance ordExtended :: Ord a => Ord (Extended a) where
  compare (Infinite) (Infinite) = EQ
  compare (Finite _) (Infinite) = LT
  compare (Infinite) (Finite _) = GT
  compare (Finite a) (Finite b) = compare a b

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f init (NonEmpty a as) = f a (foldr f init as)
  foldl f init (NonEmpty a as) = foldl f (f init a) as
  foldMap f (NonEmpty a as) = f a <> foldMap f as

data OneMore f a
  = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f init (OneMore x xs) = f x $ foldr f init xs
  foldl f init (OneMore x xs) = foldl f (f init x) xs
  foldMap f (OneMore x xs) = f x <> foldMap f xs

derive instance eqPoint :: Eq Point

derive instance ordPoint :: Ord Point

derive instance eqShape :: Eq Shape

derive instance ordShape :: Ord Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum xs = case maximum xs of
  Just x -> x

class
  Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply
  = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply a) b = a * b

instance actionMultiplyString :: Action Multiply String where
  act (Multiply n) s = power s n

instance actionArray :: Action m a => Action m (Array a) where
  act m xs = map (act m) xs

newtype Self m
  = Self m

derive newtype instance showSelf :: Show a => Show (Self a)

derive newtype instance eqSelf :: Eq a => Eq (Self a)

derive newtype instance showMultiply :: Show Multiply

derive newtype instance eqMultiply :: Eq Multiply

instance actionSelf :: Monoid m => Action m (Self m) where
  act m' (Self m'') = Self $ m' <> m''

arrayHasDuplicates :: forall a. Eq a => Hashable a => Array a -> Boolean
arrayHasDuplicates xs = length xs /= length dedup
  where
  dedup :: Array a
  dedup = nubByEq eq' xs

  eq' :: Hashable a => Eq a => a -> a -> Boolean
  eq' a b = hashEqual a b && a == b

newtype Hour
  = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashableHour :: Hashable Hour where
  hash (Hour n) = hash $ mod n 12
