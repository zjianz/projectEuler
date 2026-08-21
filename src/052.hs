import System.CPUTime (getCPUTime)
import Data.List (sort)

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

isPerm :: Integer -> Bool
isPerm n = foldl1 (&&) [equiv n (n*k) | k <- [2..6]]
    where
        equiv x y = (sort $ show x) == (sort $ show y)

result :: Integer
result = head . filter isPerm $ concatMap (\(n::Int) -> takeWhile (\x -> 6*x < 10^n) [10^(n-1)..]) [1..]

------------------------------------------------------------

main :: IO ()
main = time result