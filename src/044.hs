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

-- (a-b)(3a+3b-1) = k(3k-1)
-- 3m + n = 6a-1
-- n > 3m
-- m < k

penta :: Int -> Int
penta n = n*(3*n-1)

expressAsDiff :: Int -> [(Int, Int)]
expressAsDiff k = [ (a,b) | m <- [1..k-1],
                            let p = penta k,
                            p `mod` m == 0,
                            let n = p `div` m,
                            (3*m+n+1) `mod` 6 == 0,
                            let a = (3*m+n+1) `div` 6,
                            let b = a - m]


pivot :: Int -> Int
pivot k = foldl' scaner 0 smaller
    where
        express = expressAsDiff k
        bigger = map fst express
        smaller = map snd express
        scaner mem new
            | new `elem` bigger = update mem new
            | otherwise         = mem
        update 0 new   = new
        update mem new = min mem new 

result :: Int
result = (penta b - penta a) `div` 2
    where
        a = head $ filter ((>0) . pivot) [1..]
        b = pivot a

------------------------------------------------------------

main :: IO ()
main = time result