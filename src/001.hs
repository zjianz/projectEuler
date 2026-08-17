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
result = sum (filter test [1..999])
    where test p
            | p `mod` 3 == 0 = True
            | p `mod` 5 == 0 = True
            | otherwise     = False

------------------------------------------------------------

main :: IO ()
main = time result