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

primes :: [Integer]
primes = 2 : filter isPrime [3,5..]
    where isPrime n = 
            all (\p -> n `mod` p /= 0)
                (takeWhile (\p -> p * p <= n) primes)

-- Eratosthenes sieve not performing well
-- primes = sieve [2..]
--     where sieve (p:ps) =
--             p : sieve [x | x <- ps, x `mod` p /= 0]


result :: Integer
result = primes !! 10000

------------------------------------------------------------

main :: IO ()
main = time result

