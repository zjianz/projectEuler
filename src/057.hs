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

result :: Int
result = length . (filter (\(a,b) -> len a > len b)) . (scanl iter (3,2)) $ [1..1000]
    where
        len = length . show
        iter (a,b) _ = (a+2*b,a+b)


------------------------------------------------------------

main :: IO ()
main = time result