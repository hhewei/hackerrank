module Main where

import Control.Monad (replicateM_)

sfoc :: String -> Bool
sfoc s = sfoc' s 0 0 0 0
  where
    sfoc' :: String -> Int -> Int -> Int -> Int -> Bool
    sfoc' [] r g y b
      | r /= g = False
      | y /= b = False
      | abs (r - g) > 1 = False
      | abs (y - b) > 1 = False
      | otherwise = True

    sfoc' (c:cs) r g y b
      | abs (r - g) > 1 = False
      | abs (y - b) > 1 = False
      | c == 'R' = sfoc' cs (r+1) g y b
      | c == 'G' = sfoc' cs r (g+1) y b
      | c == 'Y' = sfoc' cs r g (y+1) b
      | c == 'B' = sfoc' cs r g y (b+1)
      | otherwise = undefined

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    s <- getLine
    print $ sfoc s
