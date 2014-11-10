module Main where

compress :: String -> Integer
compress cs = toInteger . sum $ map (\c -> fromEnum c - fromEnum '0') cs

superDigit :: Integer -> Integer
superDigit i
  | i < 10 = i
  | otherwise = superDigit . compress$ show i

main :: IO ()
main = do
  [n, k] <- words `fmap` getLine
  let n' = compress n
      k' = read k
  print$ superDigit (n' * k')
