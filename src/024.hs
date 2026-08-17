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

factorial :: Int -> Integer
factorial n = product [1..toInteger n]

gen :: Integer -> [Int] -> [Int]
gen _ [x] = [x]
gen index digits = ni : gen newindex newdigits
    where
        n  = length digits
        fn = factorial (n-1)
        ni = digits !! fromInteger (index `div` fn)
        newindex = index `mod` fn
        newdigits = filter (/= ni) digits



result :: [Int]
result = gen 999999 [0..9]

------------------------------------------------------------

main :: IO ()
main = time result