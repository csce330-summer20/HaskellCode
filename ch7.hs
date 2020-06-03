
--mapfilter
mapfilter :: (a->b) -> (a-> Bool) -> [a] -> [b]
mapfilter f p xs = map f (filter p xs)

mapfilter' f p  = (map f ).(filter p)

mapf :: (a->b) -> [a] -> [b]
mapf f as = foldr (\ x xs -> f x : xs ) [] as

filterf :: (a -> Bool) -> [a] -> [a]
filterf p xs = foldr ( \x xs -> ( if p x then [x] else [] ) ++ xs ) [] xs 

filterf' p xs = foldr ( \x xs -> ( pKeepOrNot x ++ xs) ) [] xs
    where
        pKeepOrNot x = if (p x) then [x] else [] 
