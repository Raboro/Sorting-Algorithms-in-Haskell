bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort (x:xs) = bubbleSort' (x:xs) (length (x:xs))

bubbleSort' :: Ord a => [a] -> Int -> [a]
bubbleSort' (x:xs) l 
 | l == 0 = (x:xs)
 | otherwise = bubbleSort' (bubbleSort'' (x:xs) 0) (l-1)

bubbleSort'' :: Ord a => [a] -> Int -> [a]
bubbleSort'' (x:xs) p
 | p+1 == len = list
 | otherwise = bubbleSort'' (bubbleSort''' list p)  (p+1)
   where 
    list = (x:xs)
    len = length list

bubbleSort''' :: Ord a => [a] -> Int -> [a]
bubbleSort''' (x:xs) p
 | list !! p <= list !! (p+1) = list
 | otherwise = before ++ swappedValues ++ after
   where 
    list = (x:xs)
    len = length list
    before = constructSubList list 0 p
    swappedValues = [list !! (p+1)] ++ [list !! p]
    after = constructSubList list (p+2) len

constructSubList :: Ord a => [a] -> Int -> Int -> [a]
constructSubList (x:xs) start end
 | start == end = []
 | otherwise = (list !! start) : constructSubList list (start+1) end
   where
    list = (x:xs)