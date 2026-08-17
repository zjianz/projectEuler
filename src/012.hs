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
primes = 2 : (filter isPrime [3,5..])
    where isPrime n = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)

nFactors :: Integer -> Integer
nFactors = go primes
    where
        go _ 1 = 1
        go (p:ps) n
            | p * p > n      = 2
            | n `mod` p /= 0 = go ps n
            | otherwise      = 
                let (k,l) = peel p n 1
                in k * go ps l
        peel p n k
            | n `mod` p /= 0 = (k, n)
            | otherwise      = peel p (n `div` p) (k+1)

result :: Integer   
result = go 1 nfac nfac
    where
        threshold = 500
        go k (x1:_:x3:l1) (x2:l2)
            | x1 * x2 > threshold = k * (2 * k - 1)
            | x3 * x2 > threshold = k * (2 * k + 1)
            | otherwise = go (k+1) (x3:l1) l2
        nfac = map nFactors [1..]

------------------------------------------------------------

main :: IO ()
main = time result