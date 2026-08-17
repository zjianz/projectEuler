import System.CPUTime (getCPUTime)
import Control.Monad (forM_, when)
import Control.Monad.ST (runST, ST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)


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

sieveSum :: Int -> Int
sieveSum n = runST $ do
    let bound = n
    isPrime <- newArray (2, bound) True :: ST s (STUArray s Int Bool)
    -- 0 和 1 不是素数，但我们从 2 开始
    let limit = floor (sqrt (fromIntegral bound :: Double))
    forM_ [2..limit] $ \i -> do
        prime <- readArray isPrime i
        when prime $
            forM_ [i*i, i*i+i .. bound] $ \j ->
                writeArray isPrime j False
    -- 收集素数并求和
    let go acc i
            | i > bound = return acc
            | otherwise = do
                p <- readArray isPrime i
                if p then go (acc + i) (i+1) else go acc (i+1)
    go 0 2

result :: Int
result = sieveSum 2000000

------------------------------------------------------------

main :: IO ()
main = time result