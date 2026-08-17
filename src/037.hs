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
primes = 2 : sieve [3,5..]
    where sieve (p:ps) = p : sieve (filter (\x -> p * p > x || x `mod` p /= 0) ps)

isPrime :: Integer -> Bool
isPrime n 
    | n > 1 = and [n `mod` p /= 0 | p <- takeWhile (\x -> x*x <= n) primes]
    | otherwise = False

isLPrime :: Integer -> Bool
isLPrime 3 = True
isLPrime 7 = True
isLPrime n
    | n < 10 = False
    | otherwise = isLPrime (drophead n) && isPrime n
    where 
        drophead :: Integer -> Integer
        drophead k = read (tail $ show k)

rPrimes :: [[Integer]]
rPrimes = [2,3,5,7] : map (\x -> concatMap gen x) rPrimes
    where
        gen x = [ 10*x+n | n <- [1,3,7,9],
                           isPrime (10*x+n)]

result :: Integer
result = sum $ take 11 $ filter isLPrime (concat (drop 1 rPrimes))

------------------------------------------------------------

main :: IO ()
main = time result