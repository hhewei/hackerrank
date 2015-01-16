module Main where

import Data.IntMap (IntMap, empty, insertWith, toAscList, findWithDefault)
import Data.List (intersperse)

count :: [Int] -> IntMap Int
count ns = foldl f empty ns
  where f p n = insertWith (+) n 1 p

main :: IO ()
main = do
  _ <- getLine
  ns <- words `fmap` getLine
  _ <- getLine
  ms <- words `fmap` getLine
  let l1 = map read ns
      l2 = map read ms
      c1 = count l1
      c2 = count l2
      al = toAscList c2
      diff (x,n) = if (n - findWithDefault 0 x c1 > 0) then [x] else []
      res = concat $ map diff al
  putStrLn . concat . intersperse " " $ map show res
