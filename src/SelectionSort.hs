selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort (x:xs) = lowest : selectionSort (withoutLowest (x:xs) lowest)
                       where 
                        lowest = minimum (x:xs)

withoutLowest :: Ord a => [a] -> a -> [a]
withoutLowest [] _ = []
withoutLowest (x:xs) lowest 
 | x == lowest = xs
 | otherwise = x : withoutLowest xs lowest

selectionSortHighestFirst :: Ord a => [a] -> [a]
selectionSortHighestFirst [] = []
selectionSortHighestFirst (x:xs) = highest : selectionSortHighestFirst (withoutHighest (x:xs) highest)
                                   where 
                                    highest = maximum (x:xs)

withoutHighest :: Ord a => [a] -> a -> [a]
withoutHighest [] _ = []
withoutHighest (x:xs) highest 
 | x == highest = xs
 | otherwise = x : withoutHighest xs highest

selectionSortChoose :: Ord a => [a] -> Bool -> [a]
selectionSortChoose [] _ = []
selectionSortChoose (x:xs) selection = if selection then selectionSort (x:xs) else selectionSortHighestFirst (x:xs)