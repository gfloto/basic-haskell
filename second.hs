linspace :: (Num x, Ord x) => x -> x -> [x]
linspace b e
  | b == e = [e]
  | otherwise = b:linspace (b+1) e

list :: [x] -> [x]
list [] = []
list (x:xs) = x:list xs

double :: (Num x) => x -> x
double x = 2 * x

sum' :: (Num x) => [x] -> x
{- sum' = foldl (+) 0 -}
sum' = foldl (\acc x -> acc + x) 0
{- scan is like a fold, but returns list of all values -}


main = do
  let out = linspace 4 12

  {- print(map double [1..5]) -}
  {- print(filter (>3)  [1..10]) -}

  {- print(map (\x -> x +10) [1..5]) -}
  print(map (abs . double) [-5..5])

