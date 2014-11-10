module Main where

f :: Int -> Int -> Int -> Bool
f i j n = f' (j-50) (63-15-i) 0 where
  f' x y d
    | d >= n = False
    | y > lvl = f' (if x < 0 then x+lvl else x-lvl) (y-lvl-(lvl`div`2)) (d+1)
    | y <= 0 = x == 0
    | otherwise = abs(x) == y
    where lvl = 16 `div` (product $ replicate d 2)

main :: IO ()
main = do
  n <- readLn
  mapM_ putStrLn [[if f i j n then '1' else '_' | j <- [1..100]] | i <- [1..63]]
