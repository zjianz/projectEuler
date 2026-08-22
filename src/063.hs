import System.CPUTime (getCPUTime)

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
result = length [1 | i <- [1..9], j <- takeWhile (\x -> i^x >= 10^(x-1)) [1..]]

------------------------------------------------------------

main :: IO ()
main = time result