import System.CPUTime (getCPUTime)
import Euler (isPrime, primes)

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
result = head . filter (\p -> test0 p || test1 p) $ primes
    where
        switch i j = map (\x -> if x == i then j else x)
        test0 p = '0' `elem` digits && length (filter (isPrime . read) [switch '0' c digits | c <- "123456789"]) >= 7
            where digits = show p 
        test1 p = '1' `elem` digits && length (filter (isPrime . read) [switch '1' c digits | c <- "23456789"]) >= 7
            where digits = show p 

------------------------------------------------------------

main :: IO ()
main = time result