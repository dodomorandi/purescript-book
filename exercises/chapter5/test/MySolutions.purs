module Test.MySolutions where

import Prelude
import ChapterExamples (Amp(..), Volt(..))
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Person (Person)
import Data.Picture (Bounds, Picture, Point, Shape(..), bounds, intersect, origin)
import Data.Picture as Pic

factorial :: Int -> Int
factorial = factorial' 1
  where
  factorial' :: Int -> Int -> Int
  factorial' acc 0 = acc

  factorial' acc 1 = acc

  factorial' acc n = factorial' (acc * n) (n - 1)

binomial :: Int -> Int -> Int
binomial _n 0 = 1

binomial n k
  | n < k = 0
  | n == k = 1
  | otherwise = binomial' 1 n k
    where
    binomial' acc n' k'
      | n' == k' = acc / factorial k'
      | otherwise = binomial' (acc * n') (n' - 1) k'

pascal :: Int -> Int -> Int
pascal _n 0 = 1

pascal n k
  | n < k = 0
  | otherwise = pascal (n - 1) k + pascal (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: cityA } } { address: { city: cityB } } = cityA == cityB

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [ x ] = x

fromSingleton default _ = default

circleAtOrigin :: Shape
circleAtOrigin = Circle center radius
  where
  center :: Point
  center = { x: 0.0, y: 0.0 }

  radius = 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ radius) = Circle origin (radius * 2.0)

doubleScaleAndCenter (Rectangle _ width height) = Rectangle origin (width * 2.0) (height * 2.0)

doubleScaleAndCenter (Line start end) = Line newStart newEnd
  where
  { x: startX, y: startY } = start

  { x: endX, y: endY } = end

  halfX = endX - startX

  halfY = endY - startY

  newStart :: Point
  newStart = { x: -halfX, y: -halfY }

  newEnd :: Point
  newEnd = { x: halfX, y: halfY }

doubleScaleAndCenter (Text _ text) = Text origin text

shapeText :: Shape -> Maybe String
shapeText (Text _ text) = Just text

shapeText _ = Nothing

newtype Watt
  = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp amp) (Volt volt) = Watt $ amp * volt

area :: Shape -> Number
area (Circle _ radius) = radius * radius * Number.pi

area (Rectangle _ w h) = w * h

area (Line _ _) = 0.0

area (Text _ _) = 0.0

data Clipped
  = Clipped Picture Point Number Number
  | Shape Shape

shapeBounds :: Clipped -> Bounds
shapeBounds (Clipped picture rectOrigin rectWidth rectHeight) = intersect rectangleBounds $ bounds picture
  where
  rectangleBounds = Pic.shapeBounds $ Rectangle { x: rectOrigin.x, y: rectOrigin.y } rectWidth rectHeight

shapeBounds (Shape shape) = Pic.shapeBounds shape
