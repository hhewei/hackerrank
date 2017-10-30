module Main where

import Data.List (nub, minimumBy, delete)
import Control.Monad (replicateM)

type Point = (Int, Int)
type Segment = (Point, Point)
type Vector = Point

chull :: [Point] -> [Point]
chull ps = convexHull [pvt] (pvt `delete` ps) where
  pvt = minimum ps

  convexHull cur [] = cur
  convexHull cur@(t:_) rest =
    let next = minimumBy (leftOf t) rest in
     if t /= pvt && leftOf t pvt next == LT then cur
     else convexHull (next:cur) (next `delete` rest)

leftOf :: Point -> Point -> Point -> Ordering
leftOf s a b = case 0 `compare` crossProduct (vector s a) (vector s b) of
  EQ -> distance s a `compare` distance s b
  o -> o

vector :: Point -> Point -> Vector
vector (x1,y1) (x2,y2) = ((x2 - x1), (y2 - y1))

crossProduct :: Vector -> Vector -> Integer
crossProduct (x1,y1) (x2,y2) =
  (fromIntegral x1)*(fromIntegral y2) - (fromIntegral x2)*(fromIntegral y1)

distance :: Point -> Point -> Double
distance (x1,y1) (x2,y2) = sqrt$ dx*dx + dy*dy where
  dx = fromIntegral$ x1 - x2
  dy = fromIntegral$ y1 - y2

main :: IO ()
main = do
  n <- readLn
  ps <- replicateM n $ do
    l <- getLine
    let [x, y] = map read $ words l
    return (x,y)
  let ps' = nub ps
      ch = chull ps'
  if length ch == length ps'
    then putStrLn "NO"
    else putStrLn "YES"
