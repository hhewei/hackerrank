module Main where

main :: IO ()
main = do
  l <- getLine
  putStrLn . reverse $ foldl (\s a -> if a `elem` s then s else a:s) "" l
