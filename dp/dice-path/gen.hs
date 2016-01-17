import Text.Printf (printf)
import Control.Monad (forM_)

main :: IO ()
main = do
  print 3600
  let s = [ (i,j) | i <- [1..60], j <- [1..60] ] :: [(Int, Int)]
  forM_ s $ \(i,j) -> do
    printf "%d %d\n" i j
