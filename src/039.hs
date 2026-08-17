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

triangleNum :: Int -> Int
triangleNum n
    | odd n     = 0
    | otherwise =  sum [ length $ divcount i | i <- (takeWhile (\x -> 2 * x * (x+1) <= n) [2..])]
    where
        n2 = n `div` 2
        divcount u
            | n2 `mod` u /= 0 = []
            | otherwise       = filter (\x -> gcd u x == 1 && x `mod` 2 == 1 && n3 `mod` x == 0) [u+1..2*u-1]
            where n3 = n `div` u

result :: Int
result = fst $ foldl' cmp (0,0) ((\u -> zip u (map triangleNum u)) [1..1000])
    where
        cmp (i,di) (j,dj)
            | dj > di = (j,dj)
            | otherwise = (i,di)

------------------------------------------------------------

main :: IO ()
main = time result