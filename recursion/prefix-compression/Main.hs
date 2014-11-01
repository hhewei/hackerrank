module Main where

prefix_compress :: String -> String -> String -> (String, String, String)
prefix_compress p (x0:xs) (y0:ys) | x0 == y0 = prefix_compress (x0:p) xs ys
prefix_compress p x y = (reverse p, x, y)

main :: IO ()
main = do
  x <- getLine
  y <- getLine
  let (p, x', y') = prefix_compress [] x y
  present p
  present x'
  present y'
  where
    present s = putStr (show$ length s) >> putChar ' ' >> putStrLn s
