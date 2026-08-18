import System.CPUTime (getCPUTime)
import Euler (primes, isPrime)

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

longest :: Int -> Int -> Int
longest a b = length $ takeWhile (isPrime . toInteger) [n*n + n * a + b | n <- [0..]]

result :: Int
result = fst $ foldl' cmp (0,0) [(a*b, longest a b) | b <- takeWhile (<1000) (map fromInteger primes), a <- [-b,-b+2..b]]
    where cmp (m1, l1) (m2,l2)
            | l2 > l1 = (m2,l2)
            | otherwise = (m1, l1)

------------------------------------------------------------

main :: IO ()
main = time result