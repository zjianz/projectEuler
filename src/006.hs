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

eqn :: Integer -> Integer
eqn n = n * (3*n+2) * (n^2 - 1) `div` 12

result :: Integer
result = eqn 100

------------------------------------------------------------

main :: IO ()
main = time result