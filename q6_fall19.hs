import Data.Char

capFirstWord :: String -> String
capFirstWord (c:cs) = toUpper c : capAllButFirst cs

capAllButFirst [] = []
capAllButFirst ('.':' ':c:rest) = ('.':' ':toUpper c: capAllButFirst rest) 
capAllButFirst (c:cs) = c:capAllButFirst cs 

price :: String -> [(String,Float)] -> Float
price part prices = snd(head (filter (\(p,pr)-> p==part) prices) )

pt :: [(String,Float)]
pt = [ ("nut",3.0), ("bolt",5.25), ("widget",9.99), ("screw",7.00), ("nail",2.05)]

priceRecipe :: [(String,Float)] -> [(String, Int)] -> Float
priceRecipe table recipe = sum $ map (\ (part,qty) -> price part table * fromIntegral qty ) recipe 

priceRecipe' table recipe = foldr (\ (part,qty) costs -> price part table * fromIntegral qty +costs) 0 recipe 