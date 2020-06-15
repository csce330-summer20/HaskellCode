-- from: https://wiki.haskell.org/Fold

foldt            :: (a -> a -> a) -> a -> [a] -> a
foldt f z []     = z
foldt f z [x]    = x
foldt f z xs     = foldt f z (pairs f xs)
 
pairs            :: (a -> a -> a) -> [a] -> [a]
pairs f (x:y:t)  = f x y : pairs f t
pairs f t        = t

mergesort    :: (Ord a) => [a] -> [a]
mergesort xs = foldt merge [] [[x] | x <- xs]

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys  = ys
merge (x:xs) (y:ys)
  | x <= y = x:( merge xs (y:ys) )
  | otherwise = y:(merge (x:xs) ys)