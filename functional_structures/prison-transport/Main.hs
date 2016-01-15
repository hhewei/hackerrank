{-# LANGUAGE BangPatterns #-}

import Control.Arrow
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Maybe
import Prelude hiding (lookup, map)

{-| Represents a disjoint set of integers. -}
data IntDisjointSet = IntDisjointSet { parents :: IntMap.IntMap Int,
                                       ranks   :: IntMap.IntMap Int }

instance Show IntDisjointSet where
    show = ("fromList " ++) . show . fst . toList

{-| Create a disjoint set with no members. O(1). -}
empty :: IntDisjointSet
empty = IntDisjointSet IntMap.empty IntMap.empty

{-| Create a disjoint set with one member. O(1). -}
singleton :: Int -> IntDisjointSet
singleton !x = let p = IntMap.singleton x x
                   r = IntMap.singleton x 0
               in  p `seq` r `seq` IntDisjointSet p r

{-|
Insert x into the disjoint set.
If it is already a member, then do nothing,
otherwise x has no equivalence relations.
O(logn).
-}
insert :: Int -> IntDisjointSet -> IntDisjointSet
insert !x set@(IntDisjointSet p r) =
    let (l, p') = IntMap.insertLookupWithKey (\_ _ old -> old) x x p
    in  case l of
          Just _  -> set
          Nothing ->
              let r' = IntMap.insert x 0 r
              in  p' `seq` r' `seq` IntDisjointSet p' r'

{-|
Given two instances of disjoint sets that share no members in common,
computes a third disjoint set that is the combination of the two.
This method is unsafe in that is does not verify that the two input
sets share no members in common and in the event that a member
overlaps, the resulting set may have incorrect equivalence relations.
-}
unsafeMerge :: IntDisjointSet -> IntDisjointSet -> IntDisjointSet
unsafeMerge (IntDisjointSet p1 r1) (IntDisjointSet p2 r2) =
    IntDisjointSet (IntMap.union p1 p2) (IntMap.union r1 r2)

{-|
Create an equivalence relation between x and y.
Amortized O(logn * \alpha(n)).
This function works by looking up the set representatives
for both x and y.  If they are the same, it does nothing.
Then it looks up the rank for both representatives and
makes the tree of the smaller ranked representative a
child of the tree of the larger ranked representative.
If both representatives have the same rank, x is made a
child of y and the rank of y is increase by 1.
If either x or y is not present in the input set, nothing is done.
-}
union :: Int -> Int -> IntDisjointSet -> IntDisjointSet
union !x !y set = flip execState set $ runMaybeT $ do
  repx <- MaybeT $ state $ lookup x
  repy <- MaybeT $ state $ lookup y
  guard $ repx /= repy
  (IntDisjointSet p r) <- get
  let rankx = r IntMap.! repx
  let ranky = r IntMap.! repy
  put $! case compare rankx ranky of
    LT -> let p' = IntMap.insert repx repy p
              r' = IntMap.delete repx r
          in  p' `seq` r' `seq` IntDisjointSet p' r'
    GT -> let p' = IntMap.insert repy repx p
              r' = IntMap.delete repy r
          in  p' `seq` r' `seq` IntDisjointSet p' r'
    EQ -> let p' = IntMap.insert repx repy p
              r' = IntMap.delete repx $! IntMap.insert repy (ranky + 1) r
          in  p' `seq` r' `seq` IntDisjointSet p' r'

{-|
Find the set representative for this input.
This performs path compression and so is stateful.
Amortized O(logn * \alpha(n)).
-}
lookup :: Int -> IntDisjointSet -> (Maybe Int, IntDisjointSet)
lookup !x set =
  case find x set of
    Nothing  -> (Nothing, set)
    Just rep -> let set' = compress rep x set
                in  set' `seq` (Just rep, set')

{-| Return a list of all the elements. -}
-- This is stateful for consistency and possible future revisions.
elems :: IntDisjointSet -> ([Int], IntDisjointSet)
elems = IntMap.keys . parents &&& id

{-|
Generate an association list of each element and its representative,
in arbitrary order.
-}
toList :: IntDisjointSet -> ([(Int, Int)], IntDisjointSet)
toList set = flip runState set $ do
               xs <- state elems
               forM xs $ \x -> do
                 Just rep <- state $ lookup x
                 return (x, rep)

{-|
Given an association list representing equivalences between elements,
generate the corresponding disjoint-set.
-}
fromList :: [(Int, Int)] -> IntDisjointSet
fromList = foldr (\(x, y) -> union x y . insert y . insert x) empty

{-| True if both elements belong to the same set. -}
equivalent :: Int -> Int -> IntDisjointSet -> (Bool, IntDisjointSet)
equivalent !x !y set = first (fromMaybe False) $
                       flip runState set $
                       runMaybeT $ do
                         repx <- MaybeT $ state $ lookup x
                         repy <- MaybeT $ state $ lookup y
                         return $! repx == repy

{-| Return the number of disjoint sets. O(1). -}
disjointSetSize :: IntDisjointSet -> Int
disjointSetSize = IntMap.size . ranks

{-| Return the number of elements in all disjoint sets. O(1). -}
size :: IntDisjointSet -> Int
size = IntMap.size . parents

{-|
Map each member to another Int.
The map function must be a bijection, i.e. 1-to-1 mapping.
-}
map :: (Int -> Int) -> IntDisjointSet -> IntDisjointSet
map f (IntDisjointSet p r) =
  let p' = IntMap.fromList $ List.map (f *** f) $ IntMap.toList p
      r' = IntMap.fromList $ List.map (first f) $ IntMap.toList r
  in  p' `seq` r' `seq` IntDisjointSet p' r'

-- Find the set representative.
-- This traverses parents until the parent of y == y and returns y.
find :: Int -> IntDisjointSet -> Maybe Int
find !x (IntDisjointSet p _) =
  do x' <- IntMap.lookup x p
     return $! if x == x' then x' else find' x'
  where find' y = let y' = p IntMap.! y
                  in  if y == y' then y' else find' y'

-- Given a start node and its representative, compress
-- the path to the root.
compress :: Int -> Int -> IntDisjointSet -> IntDisjointSet
compress !rep = helper
  where helper !x set@(IntDisjointSet p r)
          | x == rep  = set
          | otherwise = helper x' set'
          where x'    = p IntMap.! x
                set'  = let p' = IntMap.insert x rep p
                        in  p' `seq` IntDisjointSet p' r

main :: IO ()
main = do
  n <- readLn
  m <- readLn
  links <- replicateM m readPair
  let ds = fromList links
      (xs, _) = List.foldl' (\(!p,!s) !i -> let (!m,!s') = lookup i s in (fromMaybe i m : p, s')) ([], ds) [1..n]
      ss = List.sort xs
      gs = List.group ss
      total = sum$ List.map (cost . length) gs
  -- print$ List.length xs
  -- print$ List.length ss
  -- print$ List.length gs
  print total
  where
    readPair :: IO (Int, Int)
    readPair = do
      [i, j] <- words `fmap` getLine
      return (read i, read j)

    cost :: Int -> Int
    cost k = head$ dropWhile (\x -> x*x < k) [1..]
