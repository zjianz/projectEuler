import System.CPUTime (getCPUTime)
import Data.Foldable (find)

time :: Show a => a -> IO ()
time x = do
    start <- getCPUTime
    print x
    end <- getCPUTime
    let diff = fromIntegral (end - start) / 1e12 :: Double
    putStrLn ("Time cost: " ++ show diff ++ "s")

------------------------------------------------------------
-- Solution
------------------------------------------------------------

limit :: Integer
limit = 1000000
result :: Integer
result = case find (\q -> (3*q) `mod` 7 == 1) [limit,limit-1..1] of
    Just q -> (3*q) `div` 7
    Nothing -> 1

------------------------------------------------------------

main :: IO ()
main = time result