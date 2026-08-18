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

isLPrime :: Integer -> Bool
isLPrime 3 = True
isLPrime 7 = True
isLPrime n
    | n < 10 = False
    | otherwise = isLPrime (drophead n) && isPrime n
    where 
        drophead :: Integer -> Integer
        drophead k = read (tail $ show k)

rPrimes :: [[Integer]]
rPrimes = [2,3,5,7] : map (\x -> concatMap gen x) rPrimes
    where
        gen x = [ 10*x+n | n <- [1,3,7,9],
                           isPrime (10*x+n)]

result :: Integer
result = sum . take 11 . filter isLPrime . concat . tail $ rPrimes

------------------------------------------------------------

main :: IO ()
main = time result