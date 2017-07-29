module Main where

import Control.Bind ((=<<))
import Control.Monad.Eff.Console (logShow)
import Data.Semigroup ((<>))

main = logShow "OK"
