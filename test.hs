module Main where

f a b = (a*2) + b

{-
main = do
    putStrLn $ show $ foldr f 0 [1..100000]
-}
main :: IO ()
main = interact (count 0)
    where count i []        = show i
          count i ('\n':xs) = count (i+1) xs
          count i (_:xs)    = count i     xs
