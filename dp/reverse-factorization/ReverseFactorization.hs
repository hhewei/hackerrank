module Main where

import Control.Monad.Trans.State.Strict
import Data.Map.Strict as M (Map, empty, lookup, insert)
import Data.Maybe (catMaybes)
import Data.List (intersperse, sort)

type Memo = Map (Int,Int) (Maybe [Int])

solve :: [Int] -> Int -> Maybe [Int]
solve as n = evalState (dp n 0) empty where
  -- dp :: Int -> Int -> State x [Int]
  dp 1 _ = return$ Just []
  dp _ s | s >= length as = return Nothing
  dp p s = do
    let (f:_) = drop s as
    m <- get
    let res = M.lookup (p,s) m
    case res of
      Just ans -> return ans
      Nothing -> do
        ma <- dp p (s+1)
        mb <- if p `mod` f == 0
              then do
                sub <- dp (p `div` f) s
                return$ (f:) `fmap` sub
              else return Nothing
        let cands = catMaybes [ma, mb]
            ans = case cands of
              a:b:_ -> Just$ better a b
              a:_ -> Just$ a
              [] -> Nothing
        let m' = insert (p,s) ans m
        put m'
        return ans

better :: [Int] -> [Int] -> [Int]
better a b | length a < length b = a
better a b | length b > length a = b
better a b = min a b

main :: IO ()
main = do
  [s1, _] <- words `fmap` getLine
  let n = read s1
  ss <- words `fmap` getLine
  let as = sort$ map read ss
  case solve as n of
    Just sol -> do
      let states = scanl (*) 1 sol
      putStrLn $ concat . intersperse " " $ map show states
    Nothing -> putStrLn "-1"
