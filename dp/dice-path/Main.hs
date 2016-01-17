import Control.Monad (replicateM_, forM_)
import Data.Map.Strict (empty, singleton, elems, toList, fromList, unionWith)
import qualified Data.Vector.Mutable as V (replicate, read, write)

data Dice = Dice {
  top :: Int,
  front :: Int,
  left :: Int
  } deriving (Eq, Ord)

bottom :: Dice -> Int
bottom s = 7 - top s
back :: Dice -> Int
back s = 7 - front s
-- right :: Dice -> Int
-- right s = 7 - left s

-- solve :: (Int, Int) -> IO (MVector (Map Dice Int))
solve (m,n) = do
  v <- V.replicate (m*n) empty
  let initial = Dice 1 2 3
  V.write v 0 $ singleton initial 1

  forM_ [1..n-1] $ \j -> do
    pre <- V.read v (j-1)
    let cur = move rotateR pre
    V.write v j cur

  forM_ [1..m-1] $ \i -> do
    pre <- V.read v ((i-1)*n)
    let cur = move rotateD pre
    V.write v (i*n) cur
    
  forM_ [ (i,j) | i <- [1..m-1], j <- [1..n-1] ] $ \(i,j) -> do
    preLeft <- V.read v (i*n+j-1)
    preUp <- V.read v ((i-1)*n+j)
    let cur1 = move rotateR preLeft
        cur2 = move rotateD preUp
        cur = unionWith max cur1 cur2
    V.write v (i*n+j) cur

  return v
  
  where
    rotateR s = Dice (left s) (front s) (bottom s)
    rotateD s = Dice (back s) (top s) (left s)
    
    move rotate pre =
      let step (dice,value) = (dice', value + top dice') where dice' = rotate dice in
      fromList . map step $ toList pre

main :: IO ()
main = do
  nc <- readLn
  dp <- solve (60,60)
  replicateM_ nc $ do
    [m, n] <- (map read . words) `fmap` getLine
    r <- V.read dp ((m-1)*60 + (n-1))
    print . maximum $ elems r
