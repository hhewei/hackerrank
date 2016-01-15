import Control.Monad (replicateM)
import Data.List (intersperse)
import Data.IntMap.Strict (fromList, toList, intersectionWith)

main :: IO ()
main = do
  n <- readLn
  maps <- replicateM n $ do
    ws <- words `fmap` getLine
    let is = map read ws
        l = length is
        primes = map snd . filter (\(idx,_) -> 1 == idx `mod` 2) $ zip [1..l] is
        powers = map snd . filter (\(idx,_) -> 0 == idx `mod` 2) $ zip [1..l] is
        m = fromList$ zip primes powers
    return m
  let res = toList$ foldl1 (intersectionWith min) maps
      strs = map (\(prime,power) -> show prime ++ " " ++ show power) res
      str = concat $ intersperse " " strs
  putStrLn str
