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

factorial :: [Integer]
factorial = scanl (*) 1 [1..]

binomial n m = factorial!!n `div` (factorial!!m * factorial!!(n-m))

limit = 1000000

result :: Int
result = (length . filter (>limit) $ [binomial n m | n <- [1..100], m <- [0..n]])

------------------------------------------------------------

main :: IO ()
main = time result