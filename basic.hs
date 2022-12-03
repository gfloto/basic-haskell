test :: [Int] -> [Int]
test x = [2*a | a <- x]

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs
{- this works because 1:2:3:[] is [1..3] -}

pattern :: String -> String
pattern "" = error "empty string"
pattern full@(x:xs) = full ++ " starts with " ++ [x]

guard :: (RealFloat  a) => a -> String
guard n
  | n > 10 = "large"
  | otherwise = "smol"

max' :: (Ord a) => [a] -> a
max' [] = error "empty list"
max' [x] = x
max' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = max' xs

main = do
  let a = [2*x | x <- [1..10], x `mod` 3 == 0]

  let x = [(a,b) | a <- [1..5], b <- [1..3]]
  let y = [a+b | (a,b) <- x]

  let k = [5, 7, 2, 10, 5, 3]

  print(max' k)



