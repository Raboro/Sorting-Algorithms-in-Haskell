import InsertionSort

shellSort :: Ord a => [a] -> [a]
shellSort [] = []
shellSort (x : xs) = shellSort' list (determineJumps (length list) 0)
  where
    list = (x : xs)

determineJumps :: Int -> Int -> Int
determineJumps len jump
  | jump > len = lowerJump jump
  | otherwise = determineJumps len (3 * jump + 1)

lowerJump :: Int -> Int
lowerJump jump = floor $ fromIntegral (jump - 1) / 3

shellSort' :: Ord a => [a] -> Int -> [a]
shellSort' (x : xs) jump
  | jump == 1 = insertionSort (x : xs)
  | otherwise = shellSort' (shellSort'' (x : xs) jump 0) (lowerJump jump)

shellSort'' :: Ord a => [a] -> Int -> Int -> [a]
shellSort'' (x : xs) jump pos
  | (jump + pos + 1) > length list = list
  | otherwise =
      if listAtPos > listAtPosJump
        then shellSort'' (swap list listAtPos listAtPosJump) jump (pos + 1)
        else shellSort'' list jump (pos + 1)
  where
    list = (x : xs)
    listAtPos = list !! pos
    listAtPosJump = list !! (pos + jump)

swap :: Ord a => [a] -> a -> a -> [a]
swap [] _ _ = []
swap (x : xs) n m
  | n == x = m : swap xs n m
  | m == x = n : swap xs n m
  | otherwise = x : swap xs n m