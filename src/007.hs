import System.CPUTime (getCPUTime)
import Euler (primes)

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

result :: Integer
result = primes !! 10000

------------------------------------------------------------

main :: IO ()
main = time result

