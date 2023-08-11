mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort evenList) (mergeSort oddList)
               where 
                (evenList, oddList) = splitInEvenAndOdd xs

splitInEvenAndOdd :: [Int] -> ([Int], [Int])
splitInEvenAndOdd [] = ([], [])
splitInEvenAndOdd [x] = ([x], [])
splitInEvenAndOdd (x:y:rest) = (x:xs, y:ys)
                          where 
                           (xs, ys) = splitInEvenAndOdd rest

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys