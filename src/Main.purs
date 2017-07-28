module Main where

import Control.Monad.Eff.Console (logShow)
import Math (sqrt)
import Prelude

diagonal w h = sqrt (w * w + h * h)

main = logShow (diagonal 3.0 4.0)
