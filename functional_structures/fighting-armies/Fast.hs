{-# LANGUAGE Strict #-}

import Prelude hiding (lookup, getLine, null)
import Data.Maybe (fromJust)
import Data.ByteString.Char8 as B (getLine, readInt, break, ByteString, drop, words)
import Data.IntMap.Strict (IntMap, empty, lookup, lookupMax, delete, adjust, unionWith, alter, update, singleton)
import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)

type Army = IntMap Int
type St = IntMap Army

type Action a = StateT St IO a

readJustInt :: ByteString -> Int
readJustInt bs = let Just (i, _) = readInt bs in i

readInts :: IO [Int]
readInts = do
  ws <- B.words `fmap` getLine
  return$ map readJustInt ws

findStrongest :: Int -> Action ()
findStrongest i = do
  Just army <- (lookup i) `fmap` get
  let Just (mc, _) = lookupMax army
  liftIO$ print mc

strongestDied :: Int -> Action ()
strongestDied i = do
  s <- get
  let Just army = lookup i s
      Just (mc, _) = lookupMax army
      f a | a == singleton mc 1 = Nothing
      f a = Just$ update dec mc a
      dec 1 = Nothing
      dec x = Just$ x - 1
      s' = update f i s
  put s'

recruit :: Int -> Int -> Action ()
recruit i c = do
  s <- get
  let inc Nothing = Just 1
      inc (Just n) = Just$ n + 1
      g Nothing = Just$ singleton c 1
      g (Just a) = Just$ alter inc c a
      s' = alter g i s
  put s'

merge :: Int -> Int -> Action ()
merge i j = do
  s <- get
  let Just a = lookup j s
      s' = delete j s
      f x = unionWith (+) x a
  put$ adjust f i s'

process :: Action ()
process = do
  (e:p1:ps) <- liftIO readInts
  case e of
    1 -> findStrongest p1
    2 -> strongestDied p1
    3 -> recruit p1 (head ps)
    4 -> merge p1 (head ps)

main :: IO ()
main = do
  [n, q] <- readInts
  let initial = empty
  (_, _) <- runStateT (replicateM_ q process) initial
  return ()
