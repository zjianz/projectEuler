import System.CPUTime (getCPUTime)
import Data.IntMap ( (!), fromList, IntMap )
import Data.List (scanl')
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

fact :: IntMap Int
fact = fromList $ zip [0..9] (scanl' (*) 1 [1..9])

result :: Int
result = sum [num | num <- [10..10^(7::Int)], num == sum (map ((fact!) . digitToInt) (show num))]

------------------------------------------------------------

main :: IO ()
main = time result