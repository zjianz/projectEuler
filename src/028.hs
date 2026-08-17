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

--   1 + 5/2 * (1+9) - 1 + 5/2 * (9+25) - 9 + ... 5/2 * (n^2+(n+2)^2) - n^2
-- = 4 * (9+25+..+n^2) + 5/2 * (1+(n+2)^2)

solve n = 4 * n * (n-1) * (n-2) `div` 6 - 4 + 5 * (1 + n*n) `div` 2

result :: Int
result = solve 1001

------------------------------------------------------------

main :: IO ()
main = time result