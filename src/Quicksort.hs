module Quicksort where

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = quicksort smaller ++ [x] ++ quicksort greater
  where
    smaller = [y | y <- xs, y <= x]
    greater = [y | y <- xs, y > x]