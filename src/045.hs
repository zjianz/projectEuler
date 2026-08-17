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

tri :: Integer -> Integer
tri n = n * (n+1)
pen :: Integer -> Integer
pen n = n * (3*n-1)

cand :: [Integer]
cand = [tri n | n <- [1,3..],
                let m1 = (41*n) `div` 72,
                let m2 = (41*n) `div` 70,
                m <- [m1..m2+1],
                pen m == tri n]

result :: Integer
result = cand!!2 `div` 2

------------------------------------------------------------

main :: IO ()
main = time result