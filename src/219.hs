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

leaves :: [Integer]
leaves = [1, 1, 1, 1] ++ zipWith (+) leaves (drop 3 leaves)

cost :: Integer -> Integer
cost n = go (n-1) leaves 0 0
    where
        go _ [] _ _ = undefined
        go left (leave:leaves0) value total
            | left < leave = total + (value + 5) * left
            | otherwise    = go (left - leave) leaves0 (value+1) (total + (value+5)*leave)

result :: Integer
result = cost 1000000000

------------------------------------------------------------

main :: IO ()
main = time result