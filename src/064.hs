import System.CPUTime (getCPUTime)
import Euler 

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
result = length . filter (odd . length . snd) $ go [1..10000] [1..]
    where
        go [] _ = []
        go (p:ps) (q:qs)
            | p == q*q  = go ps qs
            | otherwise = continuedFractionOfSqrt p : go ps (q:qs)

------------------------------------------------------------

main :: IO ()
main = time result