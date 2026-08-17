import System.CPUTime (getCPUTime)
import Data.Char (digitToInt)

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

groupLen :: [Int]
groupLen = [9 * n * 10^(n-1) | n <- [1..]]

indexParse :: Int -> (Int, Int)
indexParse n = go (n-1) 1 groupLen 
    where
        go _ _ [] = (0,0)
        go remain len (g:gs)
            | remain < g = (remain, len)
            | otherwise  = go (remain - g) (len + 1) gs

indexNum :: Int -> Int
indexNum n =
    let (remain, len) = indexParse n
        digit         = remain `mod` len
        num           = 10^(len-1) + remain `div` len
        in
            digitToInt ((show num)!!digit)

result :: Int
result = product (map (indexNum . (10^)) [0..6])

------------------------------------------------------------

main :: IO ()
main = time result