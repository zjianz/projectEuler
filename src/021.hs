import System.CPUTime (getCPUTime)
import Data.Array
import Data.List

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

limit :: Int
limit = 10000

divisorSum :: Array Int Int
divisorSum = accumArray (+) 1 (1, limit) [(i,j) | j <- [2..limit `div` 2], i <- [j*2,j*3..limit]]


result :: Int
result = sum [n + dn | n <- [1..limit], let dn = divisorSum ! n, dn > n, dn <= limit, divisorSum ! dn == n]

------------------------------------------------------------

main :: IO ()
main = time result