--import Control.Monad
-- Enter your code here. Read input from STDIN. Print output to STDOUT

f :: [Int] -> [Int]
f arr = map abs arr -- Complete this function here

-- This section handles the Input/Output and can be used as it is. Do not modify it.
main :: IO ()
main = do
   inputdata <- getContents
   mapM_ putStrLn $ map show $ f $ map (read :: String -> Int) $ lines inputdata
