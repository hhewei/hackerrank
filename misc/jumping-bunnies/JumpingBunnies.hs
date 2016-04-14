module Main where

main :: IO ()
main = do
  _ <- getLine
  ws <- words `fmap` getLine
  let js = map read ws
  print$ foldl1 lcm (js :: [Integer])
