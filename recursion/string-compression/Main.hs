module Main where

compress :: String -> String
compress [] = []
compress s@(c:_) =
  let
    (prefix, rest) = span (c==) s
    len = length prefix in
   if len <= 1 then
     c : compress rest
   else
     c : (show len) ++ compress rest

main :: IO ()
main = do
  l <- getLine
  putStrLn $ compress l
