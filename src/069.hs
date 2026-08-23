import System.CPUTime (getCPUTime)
import Euler
import Data.List (scanl')

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

-- for n = \prod_{i} p_i^{c_i}
-- phi(n)/n = \prod_{i} (1-1/p_i)
-- so we just need to product different primes until it overflow

result :: Integer
result = last . fst . span (<1000000) . scanl' (*) 1 $ primes

------------------------------------------------------------

main :: IO ()
main = time result