module Main where

import Control.Monad (replicateM)
import Data.List (delete)

type Point = (Integer, Integer)
type Polygon = [Point]
type Segment = (Point, Point)

area :: (Floating a) => Polygon -> a
area [a, b, c] = if align a b c then 0.0 else heron a b c
area polygon =
  let (triangle, polygon') = divide polygon in
  area triangle + area polygon'

heron :: (Floating a) => Point -> Point -> Point -> a
heron x y z =
  let a = dist x y
      b = dist y z
      c = dist z x
      s = 0.5 * (a + b + c) in
  sqrt$ s * (s - a) * (s - b) * (s - c)

dist :: (Floating a) => Point -> Point -> a
dist (xa, ya) (xb, yb) = sqrt . fromIntegral $ dx*dx + dy*dy
  where
    dx = xa - xb
    dy = ya - yb

divide :: Polygon -> (Polygon, Polygon)
divide ps =
  let (a, b, c) = cut $ cycle ps in
  ([a, b, c], delete b ps)

cut :: Polygon -> (Point, Point, Point)
cut (a:b:c:ps) =
  let rest = takeWhile (/= a) ps in
  if right a b c && all (not . (`inside` (a,b,c))) rest
    then (a, b, c)
    else cut (b:c:ps)

right :: Point -> Point -> Point -> Bool
right (xa,ya) (xb,yb) (xc,yc) =
  crossProduct (xb-xa, yb-ya) (xc-xb, yc-yb) <= 0

align :: Point -> Point -> Point -> Bool
align (xa,ya) (xb,yb) (xc,yc) =
  crossProduct (xb-xa, yb-ya) (xc-xb, yc-yb) == 0

crossProduct (ux,uy) (vx,vy) = ux*vy - uy*vx

inside :: Point -> (Point, Point, Point) -> Bool
inside p (a, b, c) =
  right a b p && right b c p && right c a p

main :: IO ()
main = do
  n <- readLn
  polygon <- replicateM n $ do
    l <- getLine
    let [x, y] = map read $ words l
    return (x, y)
  print$ area . reverse $ polygon
