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

update :: [Int] -> Int -> [Int]
update dp c = newDp
    where newDp = (take c dp) ++ zipWith (+) (drop c dp) newDp

result :: Int
result = c!!100 - 1
    where c = foldl' update (1 : repeat 0) [1..100]

------------------------------------------------------------

main :: IO ()
main = time result