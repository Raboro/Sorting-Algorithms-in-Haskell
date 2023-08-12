heapSort :: Ord a => [a] -> [a]
heapSort [] = []
heapSort (x : xs) = heapSort' (x : xs) index
  where
    index = (length (x : xs)) - 1

heapSort' :: Ord a => [a] -> Int -> [a]
heapSort' (x : xs) index
  | index == 0 && len == 1 = list
  | index == 0 = swap list first second
  | index == (len - 1) = heapSort' (heapify list 0 index) (index - 1)
  | otherwise = heapSort' (heapify swapped 0 index) (index - 1)
  where
    list = (x : xs)
    len = length list
    first = list !! 0
    second = list !! 1
    lastFree = list !! (index + 1)
    swapped = swap list first lastFree

swap :: Ord a => [a] -> a -> a -> [a]
swap [] _ _ = []
swap (x : xs) n m
  | n == x = m : swap xs n m
  | m == x = n : swap xs n m
  | otherwise = x : swap xs n m

heapify :: Ord a => [a] -> Int -> Int -> [a]
heapify (x : xs) index len
  | index == (length (x : xs)) - 1 = (x : xs)
  | otherwise = heapify (heapify' (x : xs) index len) (index + 1) (len - 1)

heapify' :: Ord a => [a] -> Int -> Int -> [a]
heapify' (x : xs) index len
  | index == ((length (x : xs)) - 1) = (x : xs)
  | otherwise =
      if leftExists
        then
          ( if rightExists
              then
                if leftElement > rightElement
                  then
                    ( if leftElement > root
                        then swap list leftElement root
                        else list
                    )
                  else
                    ( if leftElement <= rightElement
                        then
                          ( if rightElement > root
                              then swap list rightElement root
                              else list
                          )
                        else list
                    )
              else
                ( if leftElement > root
                    then swap list leftElement root
                    else list
                )
          )
        else
          ( if rightExists
              then
                ( if rightElement > root
                    then swap list rightElement root
                    else list
                )
              else list
          )
  where
    list = (x : xs)
    root = list !! index
    leftExists = ((index * 2) + 1) <= len
    rightExists = ((index * 2) + 2) <= len
    leftElement = list !! ((index * 2) + 1)
    rightElement = list !! ((index * 2) + 2)
