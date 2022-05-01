module Test.MySolutions where

import Prelude
import Data.Number (sqrt, pi)
import Data.Int (rem)

diagonal w h = sqrt $ w * w + h * h

circleArea r = r * r * pi

leftoverCents n = n `rem` 100
