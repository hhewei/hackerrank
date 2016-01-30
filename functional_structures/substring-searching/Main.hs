import Data.Vector.Unboxed as U (fromList, Vector, length, (!))
import Data.Vector.Unboxed.Mutable as M (new, IOVector, write, read)
import Control.Monad (replicateM_)

kmp_table :: Vector Char -> IO (IOVector Int)
kmp_table w = do
  let len = U.length w
  t <- new (max len 2)
  write t 0 (-1)
  write t 1 0
  
  let loop pos _ | pos >= len = return t
      loop pos cnd | (w!(pos-1)) == (w!cnd) = do
        write t pos (cnd+1)
        loop (pos+1) (cnd+1)
      loop pos cnd | cnd > 0 = do
        fallback <- M.read t cnd
        loop pos fallback
      loop pos 0 = do
        write t pos 0
        loop (pos+1) 0
        
  loop 2 0

kmp s w = do
  t <- kmp_table w
  let lenS = U.length s
      lenW = U.length w
      
      loop :: Int -> Int -> IO Bool
      loop m i | m + i >= lenS = return False
      loop m i | (w!i) == (s!(m+i)) = do
        if i == lenW - 1
          then return True
          else loop m (i+1)
      loop m i = do
        h <- M.read t i
        if h > (-1)
          then loop (m+i-h) h
          else loop (m+1) 0

  loop 0 0

main :: IO ()
main = do
  nc <- readLn
  replicateM_ nc $ do
    text <- fromList `fmap` getLine
    pat <- fromList `fmap` getLine
    res <- kmp text pat
    if res
      then putStrLn "YES"
      else putStrLn "NO"
