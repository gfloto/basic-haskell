-- data types in haskell
data Shape = Circle Float | Square Float | Elipse Float Float deriving (Show)

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Square w) = w^2
area (Elipse a b) = pi * a * b

-- one way to make data type
data  Movie_ = Movie_ String Float Int deriving (Show)

-- a better way to make data types
data Movie = Movie { title :: String,
                    revenue :: Float,
                    year :: Int
                  } deriving (Show) -- or other instances too!

-- check how map function work (dictionary)
--never add typeclass contraints in data declariations (put them in function defs)

data Day = Mon | Tues | Wed | Thurs | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

type String_ = [Char] -- type synonym

main = do
  let c = Circle 5
  print $ area c

  let m_ = Movie_ "Matrix" 10 2001
  let m = Movie "Matrix" 10 2001

  print m_
  print m
  print "more detailed printing"
