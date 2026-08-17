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
result = foldl' (+) 0 (takeWhile (<2000000) primes)
    where 
        primes = 2 : (filter isPrime [3,5..])
        isPrime n = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)

------------------------------------------------------------

main :: IO ()
main = time result