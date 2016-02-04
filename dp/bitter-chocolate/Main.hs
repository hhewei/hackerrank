import Control.Monad (replicateM_, foldM)
import Data.Vector.Unboxed.Mutable as M (new, IOVector, read, write)

data Chocolate = Chocolate {
  row1 :: Int,
  row2 :: Int,
  row3 :: Int
  }

base :: Int
base = 32

trans :: Chocolate -> Int
trans s =
  let r1 = row1 s
      r2 = row2 s
      r3 = row3 s in
  (r1*base+r2)*base+r3

bites :: Chocolate -> [Chocolate]
bites c =
  let r1 = row1 c
      r2 = row2 c
      r3 = row3 c in
  [ bite1 i c | i <- [1..(r1-1)] ] ++
  [ bite2 i c | i <- [0..(r2-1)] ] ++
  [ bite3 i c | i <- [0..(r3-1)] ]

bite1 :: Int -> Chocolate -> Chocolate
bite1 i c =
  let r1 = row1 c
      r2 = row2 c
      r3 = row3 c in
  Chocolate (min i r1) (min i r2) (min i r3)

bite2 :: Int -> Chocolate -> Chocolate
bite2 i c =
  let r1 = row1 c
      r2 = row2 c
      r3 = row3 c in
  Chocolate r1 (min i r2) (min i r3)

bite3 :: Int -> Chocolate -> Chocolate
bite3 i c =
  let r1 = row1 c
      r2 = row2 c
      r3 = row3 c in
  Chocolate r1 r2 (min i r3)
  
type Memo = IOVector Int

solve :: Memo -> Chocolate -> IO Bool
solve dp s = do
  let i = trans s
  r <- M.read dp i
  case r of
    1 -> return True -- Win
    2 -> return False -- Lose
    0 -> do
      let f p c = do
            x <- solve dp c
            return $ min p x
      lose <- foldM f True (bites s)
      if lose
        then write dp i 2
        else write dp i 1
      return$ not lose

main :: IO ()
main = do
  nc <- readLn
  dp <- new (base*base*base)
  let bitter = Chocolate 1 0 0
  write dp (trans bitter) 2
  replicateM_ nc $ do
    [r1, r2, r3] <- (map Prelude.read . words) `fmap` getLine
    ans <- solve dp $ Chocolate r1 r2 r3
    if ans
      then putStrLn "WIN"
      else putStrLn "LOSE"
