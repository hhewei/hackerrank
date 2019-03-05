{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (sort)
import Data.Map.Strict (Map, singleton, elems, assocs, fromAscListWith)

solve :: [Int] -> Int
solve [_] = 1
solve [_, _] = 2
solve (d:ds) = (1 + length ds) +
  case dropWhile (==d) ds of
    [] -> 0
    (d':rest) -> dp (d,d') rest

dp :: (Int, Int) -> [Int] -> Int
dp (x0,y0) ps =
  let i = singleton (norm(x0,y0)) 0 in
    minimum . elems $ foldl transit i ps

transit :: Map (Int, Int) Int -> Int -> Map (Int, Int) Int
transit m d =
  let as = assocs m
      f ((x,y), c) = [(norm(x, d), c + diff y d), (norm(d, y), c + diff x d)]
      as' = sort . concat $ map f as
  in fromAscListWith min as'

diff 0 0 = 0
diff x 0 = 10 - x
diff 0 y = diff y 0
diff x y = abs$ x - y

norm (x,y) | x <= y = (x,y)
norm (x,y) = (y,x)

main :: IO ()
main = do
  (_ :: Int) <- readLn
  line <- getLine
  let digits = map read $ words line
  print $ solve digits
