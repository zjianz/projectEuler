import System.CPUTime (getCPUTime)
import Control.Monad ( forM_, when )
import Data.Array.ST
    ( newListArray, readArray, writeArray, runSTUArray )
import Data.Array.Unboxed ( (!), UArray )

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

phiSieve :: Int -> UArray Int Int
phiSieve n = runSTUArray $ do
    a <- newListArray (0, n) [0..n]
    forM_ [2..n] $ \p -> do
        x <- readArray a p
        when (x == p) $ do
            forM_ [p,2*p..n] $ \q -> do
                x' <- readArray a q
                writeArray a q (x' - x' `div` p)

    return a

digi :: Int -> Int
digi 0 = 0
digi k = 7^(k`mod`10) + digi (k `div` 10)

limit :: Int
limit = 10^(7::Int)

result :: Int
result = fst $ foldl' update (0,0) [2..limit]
    where
        phi = phiSieve limit
        update (best,bestp) n
            | best == 0             = (n,np)
            | digi n /= digi np     = (best, bestp)
            | best * np > n * bestp = (n, np)
            | otherwise             = (best, bestp)
            where
                np = phi ! n

------------------------------------------------------------

main :: IO ()
main = time result