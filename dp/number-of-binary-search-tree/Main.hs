import Control.Monad (replicateM_, forM)
import Control.Monad.Primitive (PrimState)
import qualified Data.Vector.Unboxed.Mutable as V

dp :: IO (V.MVector (PrimState IO) Int)
dp = do
  v <- V.replicate 1001 0
  V.write v 0 1
  _ <- catalan v 1000
  return v
  where
    catalan v n = do
      r <- V.read v n
      if r /= 0
        then return r
        else do
        s <- forM [0..n-1] $ \i -> do
          a <- catalan v i
          b <- catalan v (n-1-i)
          return (a,b)
        let r' = modsum s
        V.write v n r'
        return r'

modsum :: [(Int, Int)] -> Int
modsum ps = foldr (\(f1,f2) p -> mb$ mb f1 * mb f2 + p) 0 ps where
  mb x = x `mod` 100000007

main :: IO ()
main = do
  n <- readLn
  table <- dp
  replicateM_ n $ do
    t <- readLn
    r <- V.read table t
    print r
