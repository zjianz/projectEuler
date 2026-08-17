import System.CPUTime (getCPUTime)
import Math.NumberTheory.ArithmeticFunctions (sigma)
import Data.Array.Unboxed ( (!), accumArray, UArray ) 

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
limit = 28123

abundant :: [Int]
abundant = filter (\n -> let s = sigma 1 n - n in s > n && s <= limit) [1..limit]

isAbundant :: UArray Int Bool
isAbundant = accumArray (||) False (1,limit) [(i, True) | i <- abundant]

isSum :: Int -> Bool
isSum n = go abundant
    where
        go [] = False
        go (a:as)
            | a > n - a = False
            | isAbundant ! (n-a) = True
            | otherwise     =  go as

result :: Int
result = sum $ filter (not . isSum) [1..limit]

------------------------------------------------------------

main :: IO ()
main = time result