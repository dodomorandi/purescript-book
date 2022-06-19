module Test.MySolutions where

import Prelude
import Control.Monad.ST (for, run)
import Control.Monad.ST.Internal (new)
import Control.Monad.ST.Ref (modify, read)
import Data.Array (foldM, head, nub, sort, tail)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Number (pow)
import Effect (Effect)
import Effect.Exception (error, throwException)

third :: forall a. Array a -> Maybe a
third xs = do
  afterFirst <- tail xs
  afterSecond <- tail afterFirst
  head afterSecond

possibleSums :: Array Int -> Array Int
possibleSums xs = nub $ sort $ foldM f 0 xs
  where
  f acc n = [ acc, acc + n ]

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil

filterM f (Cons x xs) = do
  check <- f x
  if check then do
    rest <- filterM f xs
    pure $ x : rest
  else
    filterM f xs

exceptionDivide :: Int -> Int -> Effect Int
exceptionDivide a b =
  if b == 0 then
    throwException $ error "div zero"
  else
    pure $ a / b

estimatePi :: Int -> Number
estimatePi n =
  run do
    v <- new 0.0
    for 1 (n + 1) \k ->
      ( let
          k' = toNumber k
        in
          modify (\v' -> v' + (pow (-1.0) (k' + 1.0)) / (2.0 * k' - 1.0)) v
      )
    modify (mul 4.0) v

fibonacci :: Int -> Int
fibonacci n =
  run do
    ref <- new { a: 0, b: 1 }
    for 0 n \_ ->
      modify (\{ a, b } -> { a: a + b, b: a }) ref
    { a: result } <- read ref
    pure result
