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

result :: Int
result = sum $ map digitToInt $ show (product [1..100])

------------------------------------------------------------

main :: IO ()
main = time result