import Data.Maybe (catMaybes)
import Data.List (intersperse)
import Control.Monad (forM)

solve :: Int -> (Int, Int) -> (Int, Int) -> IO ()
solve 1 (ro,co) (r,c) = display [ (ro+i, co+j) | i <- [1..2], j <- [1..2], (i,j) /= (r,c) ]
solve m (ro,co) (r,c) = do
  ps <- forM [ (i,j) | i <- [0..1], j <- [0..1] ] $ \(i,j) -> do
    let base = pow (m-1)
        lr = i * base
        ur = (i+1) * base
        lc = j * base
        uc = (j+1) * base
    if lr < r && r <= ur && lc < c && c <= uc
      then do
      solve (m-1) (ro+lr,co+lc) (r-lr,c-lc)
      return$ Nothing
      else do
      let (r',c') = hole base (i,j)
      solve (m-1) (ro+lr,co+lc) (r',c')
      return$ Just (ro+lr+r', co+lc+c')
  display $ catMaybes ps

display :: [(Int, Int)] -> IO ()
display ps = do
  let str (r,c) = show r ++ " " ++ show c
      ln = concat . intersperse " " $ map str ps
  putStrLn ln

pow :: Int -> Int
pow n = product$ replicate n 2

hole :: Int -> (Int, Int) -> (Int, Int)
hole base (0,0) = (base, base)
hole base (0,1) = (base, 1)
hole base (1,0) = (1, base)
hole _ (1,1) = (1,1)

main :: IO ()
main = do
  m <- readLn
  ws <- words `fmap` getLine
  let [r, c] = map read ws
  solve m (0,0) (r,c)
