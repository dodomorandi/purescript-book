module Test.MySolutions where

import Prelude
import Control.Alternative (guard)
import Data.Array (catMaybes, cons, filter, foldl, head, length, mapMaybe, nubBy, null, snoc, tail, (..))
import Data.Foldable (minimumBy)
import Data.Int (pow)
import Data.Maybe (Maybe, fromMaybe)
import Data.Path (Path, filename, isDirectory, ls, root, size)
import Data.Traversable (maximumBy)
import Test.Examples (factors)

isEven :: Int -> Boolean
isEven n =
  if n >= 0 then
    if n == 0 then
      true
    else
      not $ isEven $ n - 1
  else
    isEven (-n)

countEven :: Array Int -> Int
countEven arr =
  if null arr then
    0
  else
    adder + countEven (fromMaybe [] $ tail arr)
  where
  even = isEven $ fromMaybe 1 $ head arr

  adder =
    if even then
      1
    else
      0

squared :: Array Number -> Array Number
squared = map $ \n -> n * n

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter $ \n -> n >= 0.0

infix 5 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = (\n -> n >= 0.0) <$?> arr

isPrime :: Int -> Boolean
isPrime n =
  n > 1
    && (eq 1 $ length $ factors n)

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct a b = do
  a' <- a
  b' <- b
  pure [ a', b' ]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a `pow` 2 + b `pow` 2 == c `pow` 2
  pure [ a, b, c ]

primeFactors :: Int -> Array Int
primeFactors n = factors n 2 []
  where
  factors :: Int -> Int -> Array Int -> Array Int
  factors n' d fs =
    if n' < 2 then
      fs
    else
      if n' `mod` d == 0 then
        factors (n' / d) d $ snoc fs d
      else
        factors n' (d + 1) fs

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

fibTailRec :: Int -> Int
fibTailRec n = fib' n 0 1
  where
  fib' :: Int -> Int -> Int -> Int
  fib' n' a b =
    if n' == 0 then
      a
    else
      fib' (n' - 1) b (a + b)

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> cons x xs) []

onlyFiles :: Path -> Array Path
onlyFiles file =
  if isDirectory file then do
    file' <- ls file
    onlyFiles file'
  else
    [ file ]

whereIs :: Path -> String -> Maybe Path
whereIs path name = head $ matches path root
  where
  matches :: Path -> Path -> Array Path
  matches path' parent =
    if filename path' == (filename parent) <> name then
      [ parent ]
    else do
      path'' <- ls path'
      matches path'' path'

type JustFile
  = { path :: Path
    , size :: Int
    }

largestSmallest :: Path -> Array Path
largestSmallest path = nubBy (\a b -> compare (filename a) (filename b)) $ catMaybes [ max path, min path ]
  where
  max :: Path -> Maybe Path
  max = getByOrd maximumBy

  min :: Path -> Maybe Path
  min = getByOrd minimumBy

  getByOrd :: ((JustFile -> JustFile -> Ordering) -> Array JustFile -> Maybe JustFile) -> Path -> Maybe Path
  getByOrd f path' = map _.path $ f cmpFile $ getFiles path'

  getFiles :: Path -> Array JustFile
  getFiles path' =
    mapMaybe
      ( \path'' ->
          map (\size -> { path: path'', size })
            $ size path''
      )
      $ onlyFiles path'

  cmpFile :: JustFile -> JustFile -> Ordering
  cmpFile a b = compare a.size b.size
