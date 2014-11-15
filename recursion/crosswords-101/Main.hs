module Main where

import Data.Maybe (catMaybes)
import Data.Map as M (Map, fromList, (!), filter, empty, mapWithKey)
import Control.Monad (replicateM)

type Board = Map (Int,Int) Char

data Orientation = Horizontal | Vertical deriving Eq

crosswords :: [String] -> Board -> [Board]
crosswords [] board
  | M.filter (== '-') board == empty = [board]
  | otherwise = []
crosswords (word:rest) board =
  let ways = [((i,j),d) | (i,j) <- coords, d <- [Horizontal, Vertical]]
      feasibles = catMaybes$ (place board word) `map` ways in
   concat$ (crosswords rest) `map` feasibles

place :: Board -> String -> ((Int,Int),Orientation) -> Maybe Board
place board word ((i,j),dir)
  | (if dir == Vertical then i else j) + length word > 10 = Nothing
  | any (False ==) $ zipWith fit ((board!) `map` slots) word = Nothing
  | otherwise = Just$ mapWithKey (\c o -> if c `elem` slots then word!!(offset$c) else o) board
  where slots = [if dir == Vertical then (i+k,j) else (i,j+k) | k <- [0..(length word - 1)]]
        fit s c = s == '-' || s == c
        offset (i',j') = if dir == Vertical then i'-i else j'-j
                                
coords :: [(Int, Int)]
coords = [(i,j) | i <- [0..9], j <- [0..9]]

main :: IO ()
main = do
  cells <- replicateM 10 getLine
  line <- map (\c -> if c == ';' then ' ' else c) `fmap` getLine
  let
    board = fromList$ coords `zip` concat cells
    candidates = words line
    (ans:_) = crosswords candidates board
  mapM_ putStrLn [[ans!(i,j) | j <- [0..9]] | i <- [0..9]]
