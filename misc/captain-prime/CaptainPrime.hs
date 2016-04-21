import Control.Monad (forM_)
import Data.Char (chr, ord)

solve :: Int -> String
solve n =
  let ds = digits n
      w = length ds
      nz = all (/=0) ds
      ls = [ num$ drop l ds | l <- [0..(w-1)] ]
      rs = [ num$ take l ds | l <- [1..w] ] in
  case (nz, all isPrime ls, all isPrime rs) of
    (True, True, True) -> "CENTRAL"
    (True, True, False) -> "LEFT"
    (True, False, True) -> "RIGHT"
    (_, _, _) -> "DEAD"

digits :: Int -> [Int]
digits 0 = [0]
digits n = reverse$ digits' n where
  digits' 0 = []
  digits' x =
    let a = x `div` 10
        b = x `mod` 10 in
    b : digits' a

num :: [Int] -> Int
num ds = read$ map (\d -> chr$ (ord '0' + d)) ds
  
isPrime :: Int -> Bool
isPrime n | n < 2 = False
isPrime n = not$ any (\d -> n `mod` d == 0) (takeWhile (\x -> x*x <= n) primes)

primes :: [Int]
primes = filterPrime [2..] 
  where filterPrime (p:xs) = 
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    n <- readLn
    putStrLn$ solve n
