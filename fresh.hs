import Data.List

test :: [Int] -> [Int]
test x = [2*a | a <- x]

sq :: (Num x) => x -> x
sq x = x * x 

sum' :: (Num x) => [x] -> x 
sum' [] = 0
sum' (x:xs) = x + sum' xs

{- how would you impliment nub function? -}
numUnique :: (Eq a) => [a] -> Int
numUnique = length . nub

{- re-implimentation of nub -}
nub_ :: (Eq x) => [x] -> [x]
nub_ x = nub' x []
  where
    nub' [] _ = []
    nub' (x:xs) ls
      | x `elem` ls = nub' xs ls
      | otherwise = x : nub' xs (x:ls)

main = do
  let a = let sq x = x * x in map sq [1..5]
  let b = (let (a,b,c) = (1,2,3) in a+b+c)
  let c = 1:2:3:[]
  
  let d = [1..7] ++ [2..8]
  print d
  print $ nub_ d
