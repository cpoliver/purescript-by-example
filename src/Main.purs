module Main where

import Control.Monad.Eff.Console (logShow)
import Math (pi)
import Prelude

sq x = x * x

circleArea r = pi * (sq r)

main = logShow (circleArea 4.2)
