--Contributed by Ron Watkins
module Main where

fib :: Int -> Integer
fib n = fib' 0 1 n

fib' :: Integer -> Integer -> Int -> Integer
fib' s t n | n == 1 = s
           | n > 0 = fib' t (s + t) (n - 1)
           | otherwise = undefined

-- This part is related to the Input/Output and can be used as it is
-- Do not modify it
main :: IO ()
main = do
    input <- getLine
    print . fib . (read :: String -> Int) $ input
