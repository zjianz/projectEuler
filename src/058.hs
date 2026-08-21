import System.CPUTime (getCPUTime)
import Euler (isPrime)

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
result = go 1 3
    where
        go edge primeCount
            | primeCount*10 < 4*edge+1 = 2 * edge + 1
            | otherwise                = go (edge+1) (primeCount + (length $ (filter (isPrime . toInteger)) $ [(2*edge+1)^2+(2*edge+2),(2*edge+1)^2+(4*edge+4),(2*edge+1)^2+(6*edge+6)]))

------------------------------------------------------------

main :: IO ()
main = time result