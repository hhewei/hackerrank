main :: IO ()
main = do
  val1 <- readLn
  val2 <- readLn
  let s = val1 + val2 :: Int
  print s
