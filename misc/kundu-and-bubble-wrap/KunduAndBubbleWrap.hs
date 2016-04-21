import Text.Printf (printf)

solve :: Int -> Double
solve n = fromIntegral n + sum [ fromIntegral p / fromIntegral (n - p) | p <- [1..(n-1)] ]

main :: IO ()
main = do
  ws <- words `fmap` getLine
  let [n, m] = map read ws
      res = solve (n * m)
  printf "%f\n" res
