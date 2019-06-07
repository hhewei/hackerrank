import Control.Monad (forM_)

readInts :: IO [Int]
readInts = do
  toks <- words `fmap` getLine
  return$ map read toks

process :: String -> IO ()
process (a:_:rest) = do
  let [b, c] = map read $ words rest
  case a of
    'Q' -> query b c
    'U' -> update b c

query :: Int -> Int -> IO ()
query l r = do
  putStr$ "query "
  print$ [l, r]

update :: Int -> Int -> IO ()
update idx val= do
  putStr$ "update "
  print$ [idx, val]

main :: IO ()
main = do
  [n] <- readInts
  nums <- readInts
  _ <- getLine
  ls <- lines `fmap` getContents
  forM_ ls process
