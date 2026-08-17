import System.CPUTime (getCPUTime)
import Data.List (sort, sortBy)

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

collatz :: Integer -> Integer
collatz n
    | even n = n `div` 2
    | odd  n = 3 * n + 1

collatzdepth :: Integer -> Integer
collatzdepth = go 1
    where
        go n 1 = n
        go n k = go (n+1) $ collatz k

threshold :: Integer
threshold = 1000000

candidates :: [Integer]
candidates = filter (\x -> x `mod` 6 /= 4) [threshold, threshold-1..threshold `div` 2]

result :: Integer
result = case sortBy test (zip candidates $ map collatzdepth candidates) of
    ((n,d):_) -> n
    where
        test (_,x) (_,y) = compare y x
------------------------------------------------------------

main :: IO ()
main = time result