import qualified Data.Vector.Mutable as V (replicate, read, write)
import Data.Map.Strict as M (empty, singleton, unionWith, mapKeys, filterWithKey, elems)
import Control.Monad (replicateM_, forM_)

solve :: (Int, Int) -> Int -> IO Int
solve (1,1) _ = return 1
solve (n,m) k = do
  v <- V.replicate (n*m) (M.empty, M.empty)

  forM_ [1..m-1] $ \j -> do
    V.write v j (M.empty, M.singleton 0 1)

  forM_ [1..n-1] $ \i -> do
    V.write v (i*m) (M.singleton 0 1, M.empty)

  forM_ [ (i,j) | i <- [1..n-1], j <- [1..m-1] ] $ \(i,j) -> do
    (south1, east1) <- V.read v ((i-1)*m+j)
    let south = unionWith add south1 (increase east1)

    (south2, east2) <- V.read v (i*m+j-1)
    let east = unionWith add east2 (increase south2)

    V.write v (i*m+j) (south, east)

  (south, east) <- V.read v (m*n-1)
  let s1 = foldl add 0 $ elems south
      s2 = foldl add 0 $ elems east
  return (s1 `add` s2)

  where
    add :: Int -> Int -> Int
    x `add` y = (x + y) `mod` 1000000007

    increase x = filterWithKey (\k' _ -> k' <= k) $ M.mapKeys (+1) x

main :: IO ()
main = do
  nc <- readLn
  replicateM_ nc $ do
    [n, m, k] <- (map read . words) `fmap` getLine
    r <- solve (n,m) k
    print r
