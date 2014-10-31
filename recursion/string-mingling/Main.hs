module Main where

mingle :: String -> String -> String
mingle [] [] = []
mingle (p:ps) (q:qs) = p : q : mingle ps qs
mingle _ _ = undefined

main :: IO ()
main = do
  p <- getLine
  q <- getLine
  putStrLn $ mingle p q
