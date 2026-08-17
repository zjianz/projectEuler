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

fib :: Int -> Int -> Int
fib a b
    | b > threshold && even a = (a + b - 1) `div` 2
    | b > threshold && even b = (a - 1) `div` 2
    | b > threshold           = (b - 1) `div` 2
    | otherwise               = fib b (a+b)
    where threshold = 4000000

result :: Int
result = fib 1 1

------------------------------------------------------------

main :: IO ()
main = time result
