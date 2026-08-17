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

limit :: Integer
limit = 10^(1000 :: Int)

result :: Int
result = go 2 1 1
    where go n f0 f1
            | f1 > limit = n
            | otherwise  = go (n+1) f1 (f0+f1)

------------------------------------------------------------

main :: IO ()
main = time result