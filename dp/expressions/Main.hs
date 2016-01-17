import Control.Monad (forM_)
import Data.IntMap as M (IntMap, lookup, singleton, toList, fromList)
import Text.Printf (printf)
import Data.List (foldl')

step :: IntMap String -> Int -> IntMap String
step m a =
  fromList . concatMap expand $ toList m
  where
    normalize i | i >= 0 = i `mod` 101
    normalize i = 101 + (i `mod` 101)

    expand (k, sol) = [
      (normalize (k+a), '+':sol),
      (normalize (k-a), '-':sol),
      (normalize (k*a), '*':sol) ]

main :: IO ()
main = do
  _ <- getLine
  (a:as) <- (map read . words) `fmap` getLine
  let m = foldl' step (singleton a []) as
      Just solution = M.lookup 0 m

  printf "%d" a
  let pairs = zip (reverse solution) as
  forM_ pairs $ \(o,n) -> do
    printf " %c %d" o n
  putStrLn ""
