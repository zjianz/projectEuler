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

b :: Integer
b = 10000000000

result :: Integer
result = foldl' (+) 0 [k^k `mod` b | k <- [1..1000]] `mod` b

------------------------------------------------------------

main :: IO ()
main = time result