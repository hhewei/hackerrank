module Main where

import Control.Monad (forM_)

type Pattern = (Int, Int) -> Bool

sierpinski :: Int -> Pattern
sierpinski 0 = let f (x,y) = (y<32) && y >= (abs x) in f
sierpinski n =
  let p = sierpinski (n-1)
      q = shrink p
      l = translate (-16,16) q
      r = translate (16,16) q
      f (x,y) = (q (x,y)) || (l (x,y)) || (r (x,y))
  in f

shrink :: Pattern -> Pattern
shrink p =
  let f (x,y) = p (2*x,2*y) in f

translate :: (Int, Int) -> Pattern -> Pattern
translate (dx,dy) p =
  let f (x,y) = p (x+dx,y-dy) in f

main :: IO ()
main = do
  n <- readLn
  let s = sierpinski n
  forM_ [0..31] $ \i -> do
    putStrLn [ if s (j,i) then '1' else '_' | j <- [-31..31]]
