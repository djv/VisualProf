module Main (main) where

--Get Number of Divisors of n
numDivs :: Integer -> Integer
numDivs n = toInteger $ length [ x | x<-[2.. ((n `quot` 2)+1)], n `rem` x == 0] + 2

--Generate a List of Triangular Values
triaList :: [Integer]
triaList =  [foldr (+) 0 [1..n] | n <- [1..]]

--The same recursive
triaList2 = go 0 1
  where go cs n = (cs+n):go (cs+n) (n+1)

--Finds the first triangular Value with more than n Divisors
sol :: Integer -> Integer
sol n = head $ filter (\x -> numDivs(x)>n) triaList2
main = print $ sol 150
