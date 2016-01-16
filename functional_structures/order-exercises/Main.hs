import Prelude hiding (lookup)
import Data.List (sort)
import Data.IntMap.Strict (IntMap, fromList, lookup, empty, elems, splitLookup, insert, union)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)

data Trend = Increasing | Decreasing deriving Eq

main :: IO ()
main = do
  [n, k] <- readNums
  as <- readNums
  let ps = scanl (+) 0 as
      ps' = zip [0..] ps
      s = fromList $ ps'
      nextHigher = adjNextTable Increasing ps'
      nextLower = adjNextTable Decreasing ps'
      
      fold :: [Int] -> (Int,Int) -> [Int]
      fold pr (_,c) | c > n = pr
      fold pr (b,c) =
        let nh = lookup c nextHigher
            nl = lookup b nextLower
            Just base = lookup b s
            Just reached = lookup c s
            diff = reached - base
            pr' = if b == c then pr else diff:pr in
        case (nh, nl) of
          (Just hi, Nothing) -> fold pr (b,hi)
          (Nothing, _) -> fold pr' (c+1, c+1)
          (Just hi, Just li) ->
            if hi < li then
              fold pr (b,hi)
            else
              fold pr' (c+1, c+1)
      
      cands = fold [] (0,0)
      ordered = take k . reverse . sort $ cands

  forM_ ordered print
  where
    readNums :: IO [Int]
    readNums = (map read . words) `fmap` getLine

    adjNextTable :: Trend -> [(Int, Int)] -> IntMap Int
    adjNextTable trend s = fst$ foldl step (empty, empty) s where
      step :: (IntMap Int, IntMap [Int]) -> (Int, Int) -> (IntMap Int, IntMap [Int])
      step (p, u) (ci, ch) =
        let (lower, same, higher) = splitLookup ch u
            same' = fromMaybe [] same
            known = if trend == Increasing then lower else higher
            knownSlots = concat$ elems known
            upd = fromList . zip knownSlots $ repeat ci
            p' = p `union` upd
            unknown = if trend == Increasing then higher else lower
            u' = insert ch (ci:same') unknown in
        (p', u')
