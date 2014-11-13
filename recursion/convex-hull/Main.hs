module Main where

import Data.List (nub, minimumBy, delete)
import Control.Monad (replicateM)
import Text.Printf

type Point = (Int, Int)
type Segment = (Point, Point)
type Vector = Point

solve :: [Point] -> Double
solve points = perimeter$ convexHull [pvt] (pvt `delete` nub points) where
  pvt = minimum points

  convexHull cur [] = cur
  convexHull cur@(t:_) rest =
    let next = minimumBy (leftOf t) rest in
     if t /= pvt && leftOf t pvt next == LT then cur
     else convexHull (next:cur) (next `delete` rest)

leftOf :: Point -> Point -> Point -> Ordering
leftOf s a b = 0 `compare` crossProduct (vector s a) (vector s b)

vector :: Point -> Point -> Vector
vector (x1,y1) (x2,y2) = ((x2 - x1), (y2 - y1))

crossProduct :: Vector -> Vector -> Integer
crossProduct (x1,y1) (x2,y2) = (fromIntegral x1)*(fromIntegral y2) - (fromIntegral x2)*(fromIntegral y1)

perimeter :: [Point] -> Double
perimeter ps@(p:_) = sum . map distance $ segments ps where
  segments ps = ps `zip` tail (cycle ps)

distance :: (Point, Point) -> Double
distance ((x1,y1), (x2,y2)) = sqrt$ dx*dx + dy*dy where
  dx = fromIntegral$ x1 - x2
  dy = fromIntegral$ y1 - y2

main :: IO ()
main = do
  n <- readLn
  points <- replicateM n $ do
    [x, y] <- words `fmap` getLine
    return (read x, read y)
  printf "%.1f\n" $ solve points
