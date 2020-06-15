import Data.List

data HTree = CNode Double Char
            | INode Double HTree HTree deriving Show


freq :: HTree -> Double
freq (CNode f _ ) = f
freq (INode f _ _) = f 

merge :: HTree -> HTree -> HTree
merge l r = INode (freq l + freq r) l r

mergeAllTrees' :: [HTree]  -> HTree 
mergeAllTrees' [t] = t
mergeAllTrees' ts = mergeAllTrees' (t:rest)
   where
       (f:s:rest) = sortOn freq ts
       t = merge f s

     
mergeAllTrees :: [HTree] -> HTree
mergeAllTrees [t] = t
mergeAllTrees ts = mergeAllTrees (t1t2:rem2)
    where
        mini1 = mini ts
        t1 = ts !! mini1
        rem1 = removei mini1 ts
        mini2 = mini rem1
        t2 = rem1 !! mini2
        rem2 = removei mini2 rem1
        t1t2 = merge t1 t2


mini :: [HTree] -> Int
mini [_] = 0
mini (t:ts) 
    | freq t < fmintail = 0
    | otherwise = minits +1
    where
        minits = mini ts
        fmintail = freq (ts !! minits)


--recursion
removei :: Int -> [a] -> [a]
removei 0 (_:xs) = xs
removei n (x:xs) = x: removei (n-1) xs

 
--list comprehension 
removei' :: Int -> [a] -> [a]
removei' n xs = [ x | (x,i)<- zip xs [0..], i /= n  ]


--other way?
removei'' :: Int-> [a] ->[a]
removei'' 0 (_:xs) = xs
removei'' n (x:xs) = [x] ++ removei'' (n-1) xs

removei''' :: Int-> [a] -> [a]
removei''' n xs = foldr (\ (x,i) rest -> ( if i==n then [] else [x] ) ++ rest ) [] (zip xs [0..]) 

removei'''' :: Int-> [a] -> [a]
removei'''' n xs  =  map fst $ filter (\xi -> snd xi /= n) (zip xs [0..]) 

removei''''' :: Int-> [a] -> [a]
removei''''' n xs = concat $ map ( \ (x,i) -> if i/=n then [x] else [] ) (zip xs [0..])

removei'''''' :: Int-> [a] -> [a]
removei'''''' n xs = take n xs ++ drop (n+1) xs

buildHTree :: [(Char,Double)] -> HTree 
buildHTree cfs = mergeAllTrees ts
    where
        ts = map (\cf -> CNode (snd cf) (fst cf) ) cfs


decodeTree :: String -> HTree -> [(Char,String)]
decodeTree pre (CNode _ c) = [(c,pre)]
decodeTree pre (INode _ l r) = dt_l ++ dt_r
    where
        dt_l = decodeTree (pre ++ "0") l
        dt_r = decodeTree (pre ++ "1") r

--minor bug: what if there's only one char? who cares...
getCodes :: [(Char,Double)] -> [(Char,String)]
getCodes cfs = decodeTree "" (buildHTree cfs)


get350Slide38 = [('A',0.35),('B',0.1),('C',0.2),('D',0.2),('_',0.15)] 

a = CNode 0.2 'A'
b = CNode 0.1 'B'
c = CNode 0.15 'C'
d = CNode 0.05 'D'
e = CNode 0.5   'E'   

abcde = [a,b,c,d,e]
