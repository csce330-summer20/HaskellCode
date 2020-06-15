--Maybe Example

average :: [Float]->Float
average xs = sum xs / (fromIntegral $ length xs)

safeAvg :: [Float]-> Maybe Float
safeAvg [] = Nothing
safeAvg xs = Just( average xs)

--q6 run ":t f"on command line
f x y = [x,y]

--run ":t q7"on command line (String in place of [Char] is fine for your answers)
q7 = ( ( 2 , "legit") , "Kim" )

--q8
nor :: Bool -> Bool -> Bool
nor False False = True
nor _ _         = False


--q9
maxOr0 :: Float->Float-> Float
maxOr0 x y
    | m >0 = m
    | otherwise = 0
    where m = max x y

--q10
addFirst2 :: Num a => [a] ->[a]
addFirst2 = (\(x:y:zs) -> (x+y):zs)

--q11
midItem :: [a] -> a
midItem = (\xs -> xs !! ( div (length xs + 1)  2 -1 )  ) 

midItem' = (\xs -> if even $ length xs then xs !! (length xs `div` 2 - 1) else xs !! (length xs `div` 2) )



--q12
noVowels :: String -> String
noVowels cs = [ c | c<-cs , not $ elem c ['a','e','i','o','u'] ]

noVowels' cs = [c | c<-cs, not $ vowel c]
    where
        vowels = ['a','e','i','o','u']
        vowel c = elem c vowels

noVowels'' cs = filter consonant cs
    where
        consonant ch = not ( elem ch ['a','e','i','o','u'] )

--q13
redact:: [a] -> String
redact xs = [ 'X' | _<-xs]

redact' xs = replicate (length xs) 'X'

redact'' xs =  map (\_->'X') xs

redact''' xs = take (length xs) (repeat 'X')

redact'''' xs = map (const 'X') xs

--q14
isEvenLength :: [a] -> Bool
isEvenLength [] = True
isEvenLength [_] = False
isEvenLength (_:_:xs) = isEvenLength xs

--alternate solution
isEvenLength' [] = True
isEvenLength' (_:xs) = isOddLength' xs

isOddLength' [] = False
isOddLength' [_] = True
isOddLength' (_:xs) = isEvenLength' xs


--rewriting length would be "legal", but a little clumsy, i.e.
length' [] = 0
length' (_:xs) = 1+ length' xs

isEvenLength'' xs = length' xs `mod` 2 == 0

--q15, could be ints... 
type Circle = ( (Float,Float) , Float)

radius :: Circle -> Float
radius (_,r) = r

center :: Circle -> (Float,Float)
center (pos,r) = pos

--or perhaps



--q16, probably will give type and have you use for 
-- you won't have to do "deriving Show" part
data List a = Nil | Cons a (List a) deriving Show

head' (Cons x _ ) = x

tail' (Cons _ t) = t

--q17
--revcat :: [String]->String
revcat css = concat $ map reverse css

revcat' css = foldr (\cs rest -> reverse cs ++ rest ) [] css

revcat'' css = foldr (\xs xss -> xs ++ xss) [] (map reverse css)

