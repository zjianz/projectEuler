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

result :: Int
result = fromInteger $ foldl' (+) 0 (takeWhile (<2000000) primes)

------------------------------------------------------------

main :: IO ()
main = time result