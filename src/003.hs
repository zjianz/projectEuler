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

largestFactor :: Int -> Int
largestFactor n = go 2
    where go q
            | q * q > n = q
            | n `mod` q == 0 = largestFactor (n `div` q)
            | otherwise = go (q + 1)


result :: Int
result = largestFactor 600851475143

------------------------------------------------------------

main :: IO ()
main = time result