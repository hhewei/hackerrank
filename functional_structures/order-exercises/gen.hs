import Control.Monad (replicateM)
import Text.Printf (printf)
import System.Random (getStdRandom, randomR)

main :: IO ()
main = do
  let n = 100000
  printf "%d %d\n" n n
  ns <- replicateM n newRandomNum
  putStrLn . unwords $ map show ns
  where
    newRandomNum :: IO Int
    newRandomNum = getStdRandom (randomR (-10000,10000))
