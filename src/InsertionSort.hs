insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insertionSort' (x:xs) []

insertionSort' :: Ord a => [a] -> [a] -> [a]
insertionSort' [] (r:rs) = (r:rs)
insertionSort' (x:xs) [] = insertionSort' xs [x]
insertionSort' (x:xs) (r:rs) = insertionSort' xs (insertIntoList (r:rs) x)

insertIntoList :: Ord a => [a] -> a -> [a]
insertIntoList [] e = [e]
insertIntoList (x:xs) e
 | e <= x = e : (x:xs)
 | e > x = x : insertIntoList xs e