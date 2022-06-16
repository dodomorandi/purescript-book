module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, PhoneNumber, address)
import Data.AddressBook.Validation (Errors, matches, nonEmpty, validateAddress)
import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Validation.Semigroup (V)

addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe (Just a) (Just b) = Just $ a + b

addMaybe _ _ = Nothing

subMaybe :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
subMaybe (Just a) (Just b) = Just $ a - b

subMaybe _ _ = Nothing

mulMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
mulMaybe (Just a) (Just b) = Just $ a * b

mulMaybe _ _ = Nothing

divMaybe :: forall a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
divMaybe (Just a) (Just b) = Just $ a / b

divMaybe _ _ = Nothing

addApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
addApply = lift2 add

subApply :: forall f a. Apply f => Ring a => f a -> f a -> f a
subApply = lift2 sub

mulApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
mulApply = lift2 mul

divApply :: forall f a. Apply f => EuclideanRing a => f a -> f a -> f a
divApply = lift2 div

combineMaybe :: forall f a. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just f) = Just <$> f

combineMaybe Nothing = pure Nothing

stateRegex :: Regex
stateRegex = unsafeRegex "^[A-z]{2}$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S" noFlags

-- validateAddressImproved :: Address -> V Errors Address
-- validateAddressImproved address = ado
--   state <- matches "State" stateRegex address.state
--   street <- matches "Street" nonEmptyRegex address.street 
--   city <- matches "City" nonEmptyRegex address.city
--   in { state, street, city }

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a =
  address <$> matches "Street" nonEmptyRegex a.street
          <*> matches "City" nonEmptyRegex a.city
          <*> matches "State" stateRegex  a.state

data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance treeGeneric :: Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
  show tree = genericShow tree

derive instance treeEq :: Eq a => Eq (Tree a)

instance functorTree :: Functor Tree where
  map _ Leaf = Leaf
  map f (Branch left a right) =
    Branch left' a' right'
      where
        left' = f <$> left
        a' = f a
        right' = f <$> right

instance foldableTree :: Foldable Tree where
  foldr _ init Leaf = init
  foldr f init (Branch left a right) = 
    foldr f afterA left
    where
      afterA = f a afterRight
      afterRight = foldr f init right
  foldl _ init Leaf = init 
  foldl f init (Branch left a right) =
    foldl f afterA right
    where
      afterA = f afterLeft a
      afterLeft = foldl f init left
  foldMap = foldMapDefaultL

instance traversableTree :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch left a right) =
    Branch <$> traverse f left
           <*> f a
           <*> traverse f right

  sequence = traverse identity

traversePreOrder :: forall m a b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf
-- traversePreOrder f (Branch left root right) = ado
--   root' <- f root
--   left' <- traversePreOrder f left
--   right' <- traversePreOrder f right
--   in Branch left' root' right' 
traversePreOrder f (Branch left root right) =
  flip Branch <$> f root
         <*> traversePreOrder f left
         <*> traversePreOrder f right

traversePostOrder :: forall m a b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch left root right) =
  flip <$> branchWithLeft
       <*> traversePostOrder f right
       <*> f root
  where
    branchWithLeft = Branch <$> traversePostOrder f left

type Person
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }
person :: String -> String -> Maybe Address -> Array PhoneNumber -> Person
person firstName lastName homeAddress phones = {
  firstName, lastName, homeAddress, phones
  }

validatePersonOptionalAddress :: Person -> V Errors Person
validatePersonOptionalAddress p =
  person <$> nonEmpty "FistName" p.firstName
         <*> nonEmpty "LastName" p.lastName
         <*> traverse validateAddress p.homeAddress
         <*> pure p.phones

sequenceUsingTraverse :: forall m t a. Applicative m => Traversable t => t (m a) -> m (t a)
sequenceUsingTraverse = traverse identity

traverseUsingSequence :: forall m t a b. Applicative m => Traversable t => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f t = sequence $ f <$> t
