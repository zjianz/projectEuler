import System.CPUTime (getCPUTime)
import Data.List (scanl', zip7)

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

cashes :: [Int]
cashes = [1,2,5,10,20,50,100,200]

update :: [Int] -> Int -> [Int]
update dp c = newDp
    where newDp = (take c dp) ++ zipWith (+) (drop c dp) newDp

result :: Int
result = foldl' update (1 : repeat 0) cashes !! 200

------------------------------------------------------------

main :: IO ()
main = time result