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

primes :: [Int]
primes = 2 : sieve [3,5..]
    where sieve (p:ps) = p : sieve (filter (\x -> p * p > x || x `mod` p /= 0) ps)

isPrime :: Int -> Bool
isPrime n 
    | n > 1 = and [n `mod` p /= 0 | p <- takeWhile (\x -> x*x <= n) primes]
    | otherwise = False

longest :: Int -> Int -> Int
longest a b = length $ takeWhile isPrime [n*n + n * a + b | n <- [0..]]

result :: Int
result = fst $ foldl' cmp (0,0) [(a*b, longest a b) | b <- takeWhile (<1000) primes, a <- [-b,-b+2..b]]
    where cmp (m1, l1) (m2,l2)
            | l2 > l1 = (m2,l2)
            | otherwise = (m1, l1)

------------------------------------------------------------

main :: IO ()
main = time result