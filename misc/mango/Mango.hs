import Data.List (sort)

solve :: Int -> Int -> [Int] -> [Int] -> Int
solve n m as hs = bsearch 1 n where
  bsearch l u | l >= u = if try u then u else 0
  bsearch l u | l+1 == u = if try u then u else bsearch l l
  bsearch l u =
    let med = (l+u) `div` 2 in
    if try med
    then bsearch med u
    else bsearch l (med-1)

  try k =
    let s = map (\(a,h) -> a + (k-1)*h) (as `zip` hs) in
    (sum . take k $ sort s) <= m

main :: IO ()
main = do
  ws1 <- words `fmap` getLine
  ws2 <- words `fmap` getLine
  ws3 <- words `fmap` getLine
  let [n, m] = map read ws1
      as = map read ws2
      hs = map read ws3
  print$ solve n m as hs
