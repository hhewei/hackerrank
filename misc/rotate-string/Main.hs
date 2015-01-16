module Main where

import Data.List (intersperse)
import Control.Monad (replicateM_)

rotations :: String -> [String]
rotations s = zipWith (++) prefixes suffixes where
  prefixes = drop 1 $ scanr (:) [] s
  suffixes = map reverse . drop 1 $ scanl (\p c -> (c:p)) [] s

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    s <- getLine
    let res = rotations s
    putStrLn . concat . intersperse " " $ res
