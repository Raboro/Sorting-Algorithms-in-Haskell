import Quicksort

bucketSort :: [Int] -> [Int]
bucketSort [] = []
bucketSort (x : xs) = rowIterate (fillDigs numbers rows) rows
  where
    numbers = [digs number | number <- (x : xs)]
    rows = length $ maximum numbers

digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

fillDigs :: [[Int]] -> Int -> [[Int]]
fillDigs (x : xs) rows = [fillDigs' y rows | y <- (x : xs)]

fillDigs' :: [Int] -> Int -> [Int]
fillDigs' (x : xs) rows
  | rows == length list = list
  | otherwise = zeroList (rows - length list) ++ list
  where
    list = (x : xs)

zeroList :: Int -> [Int]
zeroList counter
  | counter == 0 = []
  | otherwise = 0 : zeroList (counter - 1)

rowIterate :: [[Int]] -> Int -> [Int]
rowIterate (x : xs) rows
  | rows == 0 = collect (x : xs)
  | otherwise = rowIterate (sortBuckets (iterate' l l l l l l l l l l rows (x : xs)) rows) (rows - 1)
  where
    l = [[0]]

collect :: [[Int]] -> [Int]
collect = map ((read . concat) . map show)

sortBuckets :: [[Int]] -> Int -> [[Int]]
sortBuckets [] _ = []
sortBuckets (x : xs) rows = numbers
  where
    sorted = quicksort (collect (x : xs))
    numbers = [digs number | number <- sorted]

iterate' :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Int -> [[Int]] -> [[Int]]
iterate' (a : as) (b : bs) (c : cs) (d : ds) (e : es) (f : fs) (g : gs) (h : hs) (i : is) (j : js) rows [] = compress [init (a : as), init (b : bs), init (c : cs), init (d : ds), init (e : es), init (f : fs), init (g : gs), init (h : hs), init (i : is), init (j : js)]
iterate' (a : as) (b : bs) (c : cs) (d : ds) (e : es) (f : fs) (g : gs) (h : hs) (i : is) (j : js) rows (n : ns)
  | null n = iterate' a' b' c' d' e' f' g' h' i' j' rows ns
  | v == 0 = iterate' (n : a') b' c' d' e' f' g' h' i' j' rows ns
  | v == 1 = iterate' a' (n : b') c' d' e' f' g' h' i' j' rows ns
  | v == 2 = iterate' a' b' (n : c') d' e' f' g' h' i' j' rows ns
  | v == 3 = iterate' a' b' c' (n : d') e' f' g' h' i' j' rows ns
  | v == 4 = iterate' a' b' c' d' (n : e') f' g' h' i' j' rows ns
  | v == 5 = iterate' a' b' c' d' e' (n : f') g' h' i' j' rows ns
  | v == 6 = iterate' a' b' c' d' e' f' (n : g') h' i' j' rows ns
  | v == 7 = iterate' a' b' c' d' e' f' g' (n : h') i' j' rows ns
  | v == 8 = iterate' a' b' c' d' e' f' g' h' (n : i') j' rows ns
  | v == 9 = iterate' a' b' c' d' e' f' g' h' i' (n : j') rows ns
  where
    v = n !! (rows - 1)
    a' = (a : as)
    b' = (b : bs)
    c' = (c : cs)
    d' = (d : ds)
    e' = (e : es)
    f' = (f : fs)
    g' = (g : gs)
    h' = (h : hs)
    i' = (i : is)
    j' = (j : js)

compress :: [[[Int]]] -> [[Int]]
compress [] = []
compress (x : xs)
  | null x = compress xs
  | otherwise = x ++ compress xs