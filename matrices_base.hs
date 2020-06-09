type Vector = [ Double ]

type Matrix = [ Vector ]  -- row-major

sp xs ys = sum $ map (\(x,y)->x*y) (zip xs ys)

sp' xs ys = foldr (\ (x,y) xys -> x*y + xys ) 0 (zip xs ys)

validsp :: Vector -> Vector -> Bool
validsp xs ys  = length xs == length ys

matmul' a b = [ [sp ra [ rb !! j | rb <-b ] | j <- [0.. length a -1] ]   | i <- [0.. length a -1], let ra = a !! i ] 

row :: Matrix -> Int -> Vector
row = (!!)

col :: Matrix -> Int -> Vector
col a n = [ ra !! n | ra <-a ] 

matmul a b = [ map (sp (row a i)) cbs  | i<-[0..length a -1]]
    where 
        cbs = [ col b i | i<-[0.. length (b!!0)-1]]

m3x3 :: Matrix
m3x3 = [ [1,2,3],[2,1,4],[5,2,1]]

eye3x3 :: Matrix
eye3x3 = [ [1,0,0],[0,1,0],[0,0,1]]
    