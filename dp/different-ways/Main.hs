import Control.Monad (replicateM_)
import Control.Monad.Primitive (PrimState)
import qualified Data.Vector.Unboxed.Mutable as V (MVector, replicate, read, write)

m :: Int
m = 1024

solve :: V.MVector (PrimState IO) Int -> Int -> Int -> IO Int
solve _ _ k | k == 0 = return 1
solve _ n k | k == n = return 1
solve dp n k = do
  p <- V.read dp $ l (n,k)
  if p > 0
    then return p
    else do
      a <- solve dp (n-1) (k-1)
      b <- solve dp (n-1) k
      let r = a `add` b
      V.write dp (l(n,k)) r
      return r
  where
    l (i,j) = i*m + j
    x `add` y = (x + y) `mod` 100000007

main :: IO ()
main = do
  nc <- readLn
  dp <- V.replicate (m*m) (0 :: Int)
  replicateM_ nc $ do
    [n, k] <- (map read . words) `fmap` getLine
    r <- solve dp n k
    print r
