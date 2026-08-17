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

edge :: Integer
edge = 20

result :: Integer
result = (foldl' (*) 1 [edge+1..edge*2]) `div` (foldl' (*) 1 [1..edge])

------------------------------------------------------------

main :: IO ()
main = time result