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

rev :: Integer -> Integer
rev = read . reverse . show

isLychrel :: Integer -> Bool
isLychrel n = not $ go 0 (n+rev n)
    where
        go :: Int -> Integer -> Bool
        go step num
            | step > 50      = False
            | num == rev num = True
            | otherwise      = go (step+1) (num + rev num)

result :: Int
result = length . (filter isLychrel) $ [1..10000]

------------------------------------------------------------

main :: IO ()
main = time result