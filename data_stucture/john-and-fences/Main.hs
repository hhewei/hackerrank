{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (length, minimum, head, sum)
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Vector.Unboxed (Vector, generate, slice, length, minimum, minIndex, head, sum)
import Data.Sequence (Seq, singleton, empty, viewl, ViewL((:<)), (|>))

main :: IO ()
main = do
  n <- readLn
  ws <- words `fmap` getLine
  let hs :: [Int] = take n $ map read ws
      arr :: UArray Int Int = listArray (0,n-1) hs
      vec = generate n (arr!)
  print$ solve vec
  where
    solve :: Vector Int -> Int
    solve vec = loop (singleton vec) 0 where
      loop :: Seq (Vector Int) -> Int -> Int
      loop q m | q == empty = m
      loop q m =
        let s :< rest = viewl q
            total = sum s
            len = length s in
        if total <= m || len == 0 then
          loop rest m
        else if len == 1 then
          loop rest (max (head s) m)
        else
          let mini = minIndex s
              minh = minimum s
              local = minh * len
              q' = rest |> (slice 0 mini s) |> (slice (mini + 1) (len - mini - 1) s) in
          loop q' (max local m)
