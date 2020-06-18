allPos:: [Int]->Bool
allPos [] = True
allPos (x:xs)
    | x>0 = allPos xs
    | otherwise = False

allPos' = and.(map (>0))

allPos'' xs = length ( filter (<1) xs ) == 0

allPos''' xs = foldr (\ n bs -> (n>0) && bs ) True xs

triples :: [a] -> [a]
triples xs = foldr (\x xxxs-> [x,x,x]++xxxs) [] xs

triples' xs = foldr (\x xxxs-> x:x:x:xxxs) [] xs