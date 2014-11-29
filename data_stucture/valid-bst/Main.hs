module Main where

import Control.Monad (replicateM_)

isValidBST :: [Int] -> Bool
isValidBST [] = True
isValidBST (root:rest) =
  let (lhs, rhs) = span (< root) rest in
  all (> root) rhs && isValidBST lhs && isValidBST rhs

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    _ <- getLine
    line <- getLine
    let ns = map read $ words line
        res = if isValidBST ns then "YES" else "NO"
    putStrLn res
