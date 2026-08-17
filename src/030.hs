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

limit :: Int
limit = 5*9^5

result :: Int
result = sum $ filter test [2..limit]
    where test n =
            n == (sum $ map ((^5) . digitToInt) (show n))

------------------------------------------------------------

main :: IO ()
main = time result