{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (replicateM_, forM_, filterM)
import Data.Vector.Unboxed.Mutable as V (replicate, read, write)
import Data.Text as T (pack, unpack, Text, length, drop, take)

solve :: [Text] -> Text -> IO [Text]
solve ws attempt = do
  let n = T.length attempt
  dp <- V.replicate (n+1) (-1 :: Int)
  V.write dp 0 0
  forM_ [1..n] $ \i -> do
    let potentials = [ j | w <- ws, let l = T.length w; j = i - l, j >= 0, w == sub j l ]
        check k = (>=0) `fmap` V.read dp k
    cands <- filterM check potentials
    case cands of
      c:_ -> V.write dp i c
      [] -> return ()
  final <- V.read dp n
  if final >= 0 then do
    let collect 0 = return []
        collect end = do
          prev <- V.read dp end
          let w = sub prev (end - prev)
          r <- collect prev
          return$ w:r
    reverse `fmap` collect n
  else return ["WRONG", "PASSWORD"]
  where
    sub :: Int -> Int -> Text
    sub s l = T.take l $ T.drop s attempt

main :: IO ()
main = do
  nc <- readLn
  replicateM_ nc $ do
    _ <- getLine
    ws <- (map pack . words) `fmap` getLine
    attempt <- pack `fmap` getLine
    used <- solve ws attempt
    putStrLn . unwords . map unpack $ used
