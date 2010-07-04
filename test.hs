module Main where

f a b = (a*2) + b

{-
main = do
    putStrLn $ show $ foldr f 0 [1..100000]
-}
main :: IO ()
main = print . length . lines =<< readFile "/etc/dictionaries-common/words"
