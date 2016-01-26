import Control.Monad (replicateM, forM_)
import Data.IntMap as M (fromList, lookup)
import Data.IntSet as S (fromList, toList)
import Data.Maybe (catMaybes)
import Text.Printf (printf)

data Segment = Segment {
  range :: (Int, Int),
  lowest :: Int
  } deriving Show

data STree =
  Tip { self :: Segment } |
  Branch {
    self :: Segment,
    left :: STree,
    right :: STree
    }
  deriving Show

main :: IO ()
main = do
  [n, m] <- readInts
  as <- readInts
  let arr = M.fromList $ zip [0..] as
  -- print arr
  qs <- replicateM m readInts
  let ps = S.toList . S.fromList $ 0 : (n-1) : concat qs
      mkSegment (l,r) = Segment (l,r) (minimum $ catMaybes [ M.lookup k arr | k <- [l..r] ])
      segments = map mkSegment $ zip ps (tail ps)
      stree = mkSTree segments
  -- print stree
  forM_ qs $ \[l, r] -> do
    if l == r
      then do
      let Just v = M.lookup l arr
      print v
      else do
      print$ query (l, r) stree
  
  where
    readInts :: IO [Int]
    readInts = (map read . words) `fmap` getLine

    mkSTree :: [Segment] -> STree
    mkSTree segments =
      mkSTree' $ map Tip segments
      where
        mkSTree' [tree] = tree
        mkSTree' trees = mkSTree' $ mkBranches trees

        mkBranches :: [STree] -> [STree]
        mkBranches (lhs:rhs:rest) =
          let ran = (fst . range . self $ lhs, snd . range . self $ rhs)
              low = min (lowest$ self lhs) (lowest$ self rhs)
              me = Segment ran low in
          (Branch me lhs rhs) : mkBranches rest
        mkBranches [it] = [it]
        mkBranches [] = []

    query :: (Int, Int) -> STree -> Int
    query _ (Tip (Segment _ low)) = low
    query (l,r) (Branch me lhs rhs)
      | (l,r) == range me = lowest me
      | (l < r) && (l >= (fst . range $ self lhs)) && (r <= (snd . range $ self lhs)) = query (l,r) lhs
      | (l < r) && (l >= (fst . range $ self rhs)) && (r <= (snd . range $ self rhs)) = query (l,r) rhs
      | otherwise = let x = snd . range $ self lhs in min (query (l,x) lhs) (query (x,r) rhs)
