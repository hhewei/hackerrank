module Main where

import Data.Set (empty, member, insert)

nub :: String -> String
nub s = let (_, l) = foldl f (empty,[]) s in reverse l where
  f cur@(seen, partial) char
    | char `member` seen = cur
    | otherwise = (char `insert` seen, char:partial)

main :: IO ()
main = getLine >>= (putStrLn . nub)
