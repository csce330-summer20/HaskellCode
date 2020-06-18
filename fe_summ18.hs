data Play = Rock | Paper | Scissors deriving Show

beats :: Play -> Play -> Bool
beats Rock Scissors  = True
beats Scissors Paper = True
beats Paper Rock     = True
beats _       _      = False

countOnes :: String -> Int
countOnes bs = length [b | b<-bs, b=='1']

all' :: (a -> Bool) -> [a] -> Bool
all' f xs = foldr (\x bs -> f x && bs) True xs

sansNeg :: (Num a, Ord a) => [a] -> [a]
sansNeg ns = foldr (\ n ns -> if n>=0 then n:ns else ns) [] ns

sansNeg' = filter (>=0) 