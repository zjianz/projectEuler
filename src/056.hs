import System.CPUTime (getCPUTime)
import Data.Char (digitToInt)

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

maxBased :: Integer -> Int
maxBased a = go 1 a 0
    where
        go :: Int -> Integer -> Int -> Int
        go 100 _ acc = acc
        go step cur acc = go (step+1) (cur*a) (max acc (sum . (map digitToInt) . show $ cur))

result :: Int
result = maximum [maxBased a | a <- [1..100]]

------------------------------------------------------------

main :: IO ()
main = time result