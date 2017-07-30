module Data.RecursionExercises where

import Prelude

import Data.Array (null)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)

isEven :: Int -> Boolean
isEven int = mod int 2 == 0

countEven :: Array Int -> Int -> Int
countEven toCount count =
  if null toCount
    then count
    else countEven (unsafePartial tail toCount) (updateCount)
      where
        updateCount :: Int
        updateCount = 
          if isEven (unsafePartial head toCount)
            then count + 1
            else count

recurseUntilEven :: Int -> Boolean
recurseUntilEven int =
  if isEven int
    then true
    else recurseUntilEven (int - 1)
