{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.State
import Data.List (lines)
import Data.IntMap.Strict (IntMap, empty, insert, unionWith, foldlWithKey')

newtype Bound = Bound Int deriving (Show, Eq, Ord, Enum, Num)

type Factors = IntMap Int

data Tree =
  Branch !Bound !Tree !Bound !Tree !Bound !Factors
  | Leaf !Bound !Factors
  deriving (Show)

modulo :: Int
modulo = 1000000000 + 7

primes :: [Int]
primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

smallPrimes :: [Int]
smallPrimes = takeWhile (<100) primes

lcm' :: Factors -> Factors -> Factors
lcm' x y = unionWith max x y

factors :: Int -> Factors
factors x = loop x smallPrimes empty
  where
    loop 1 _ acc = acc
    loop n (p:rest) acc | n `mod` p == 0 = let (r, c) = fact n p in loop r rest (insert p c acc)
    loop n (_:rest) acc = loop n rest acc

    fact :: Int -> Int -> (Int, Int)
    fact num den = f num 0
      where f x acc | x `mod` den == 0 = f (x `div` den) (acc+1)
            f x acc = (x, acc)

(*:) :: Factors -> Int -> Factors
x *: y = unionWith (+) x $ factors y

prd :: Factors -> Int
prd fs =
  let f p k n = (p * loop k n 1) `mod` modulo in
    foldlWithKey' f 1 fs
  where
    loop k 0 p = p
    loop k n p = loop k (pred n) (p*k `mod` modulo)

readInts :: IO [Int]
readInts = do
  l <- getLine
  let ws = words l
  return$ map read ws

multiply :: Bound -> Int -> Tree -> Tree
multiply idx mul (Leaf b i) | idx == b = Leaf b (i*:mul)
multiply idx mul (Branch l lhs m rhs r i)
  | idx < m =
    let lhs' = multiply idx mul lhs
        x = root lhs'
        y = root rhs
    in Branch l lhs' m rhs r (x `lcm'` y)
  | otherwise =
    let rhs' = multiply idx mul rhs
        x = root lhs
        y = root rhs'
    in Branch l lhs m rhs' r (x `lcm'` y)

query :: Tree -> (Bound, Bound) -> Factors
query (Leaf b i) (l,u) | l == u && l == b = i
query (Branch l lhs m rhs r i) (from, to)
  | from == l && to == (pred r) = i
  | to < m = query lhs (from, to)
  | from >= m = query rhs (from, to)
  | otherwise = query lhs (from, pred m) `lcm'` query rhs (m, to)

process :: String -> StateT Tree IO ()
process (a:_:rest) =
  let [b, c] = map read $ words rest in
  case a of
    'Q' -> do
      t <- get
      let res = prd$ query t (Bound b, Bound c)
      liftIO$ print res
    'U' -> do
      modify' $ multiply (Bound b) c

root :: Tree -> Factors
root (Branch _ _ _ _ _ i) = i
root (Leaf _ i) = i

buildTree :: [Int] -> Tree
buildTree = build 0 where
  build :: Bound -> [Int] -> Tree
  build offset [n] = Leaf offset $ factors n
  build offset nums =
    let size = length nums
        r = size `div` 2
        l = size - r
        med = offset + Bound l
        upper = offset + Bound size
        (lhs, rhs) = splitAt l nums
        ltree = build offset lhs
        rtree = build med rhs
        lval = root ltree
        rval = root rtree
    in
      Branch offset ltree med rtree upper (lval `lcm'` rval)

main :: IO ()
main = do
  _ <- getLine -- N
  nums <- readInts
  let init = buildTree nums
  _ <- getLine -- K
  ls <- liftIO$ lines `fmap` getContents
  evalStateT (forM_ ls process) init
