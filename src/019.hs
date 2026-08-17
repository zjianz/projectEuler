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

mon_ord :: [Int]
mon_ord = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
mon_numa :: [Int]
mon_numa = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

year_mon :: Int -> [Int]
year_mon n
    | n `mod` 400 == 0 = mon_numa
    | n `mod` 100 == 0 = mon_ord
    | n `mod` 4   == 0 = mon_numa
    | otherwise        = mon_ord

mon_list :: [Int]
mon_list = concatMap year_mon [1901..2000]

result :: Int
result = go 1 0 mon_list
    where
        go n k (x:xs)
            | xs == []         = n
            | nk `mod` 7 == 0  = go (n+1) nk xs
            | otherwise        = go n     nk xs
            where nk = k+x

------------------------------------------------------------

main :: IO ()
main = time result