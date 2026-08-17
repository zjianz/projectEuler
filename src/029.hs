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

logFloor :: Int -> Int -> Int
logFloor n lim = go 0 1
    where go k val
            | val * n > lim = k
            | otherwise     = go (k+1) (val*n)

limit :: Int
limit = 100

fullDiv :: Int -> Int -> (Int, Int)
fullDiv k p = go k 0
    where
        go num acc
            | num `mod` p == 0 = go (num `div` p) (acc+1)
            | otherwise        = (num, acc)

isPerfectPower :: Int -> Bool
isPerfectPower n = go n 2 0
    where
        go k p d
            | d == 1         = False
            | k == 1         = d > 1
            | p * p > k      = False
            | k `mod` p /= 0 = go k (p+1) d
            | otherwise      = go res (p+1) newd
            where 
                (res, expo)  = fullDiv k p
                newd         = gcd expo d

newElemNum :: Int -> Int
newElemNum 1 = limit - 1
newElemNum n = length $ filter divtest [2..limit]
    where divtest k = all (\x -> limit*x < (n*k) || (n*k) `mod` x /= 0) [1..n-1]

elemNum :: [Int]
elemNum = scanl (+) 0 (map newElemNum [1..])

result :: Int
result = sum $ map gain [2..limit]
    where
        gain n
            | isPerfectPower n = 0
            | otherwise        = elemNum !! (logFloor n limit) 

------------------------------------------------------------

main :: IO ()
main = time result