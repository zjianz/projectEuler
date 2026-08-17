import System.CPUTime (getCPUTime)
import Control.Monad (forM_, when)
import Control.Monad.ST (runST, ST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef)

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

collatz :: Int -> Int
collatz n
    | even n = n `div` 2
    | odd  n = n * 3 + 1
    | otherwise = 1

solver :: Int -> Int
solver n = runST $ do
    let arrayLen = n
    cache <- newArray (1, n) 0 :: ST s (STUArray s Int Int)
    writeArray cache 1 1
    maxRef <- newSTRef (1,1)
    let go k ks
            | k > arrayLen = go (collatz k) (k:ks)
            | otherwise    = do
                val <- readArray cache k
                if val > 0
                    then return (k:ks)
                    else do
                        go (collatz k) (k:ks)
    forM_ [n,n-1..1] $ \i -> do
        v <- readArray cache i
        if v > 0 then return () else do
            lchain <- go i []
            base   <- readArray cache $ case lchain of 
                (k:_) -> k
                []    -> 0
            forM_ (zip lchain [base..]) $ \(k,l) -> do
                if k < arrayLen then writeArray cache k l else return ()
            (maxLen, _) <- readSTRef maxRef
            when (length lchain + base - 1 > maxLen) $
                writeSTRef maxRef (length lchain + base - 1, i)

    fst <$> readSTRef maxRef

result :: Int
result = solver 1000000

------------------------------------------------------------

main :: IO ()
main = time result