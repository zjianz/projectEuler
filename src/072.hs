import System.CPUTime (getCPUTime)
import Control.Monad ( when, foldM, forM_ )
import Data.Array.ST
    ( newListArray, readArray, writeArray, runSTUArray )
import Data.Array.Unboxed ( (!), UArray )
import Control.Monad.ST (runST)
import GHC.ST (ST(ST))
import Data.Array.Base (STUArray(STUArray))

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

phiSieve :: Int -> Int
phiSieve n = runST $ do
    a <- newListArray (0, n) [0..n] :: ST s (STUArray s Int Int)
    foldM (\s p -> do
        x <- readArray a p
        when (x == p) $ do
            forM_ [p,2*p..n] $ \q -> do
                x' <- readArray a q
                writeArray a q (x' - x' `div` p)
        nx <- readArray a p
        return (s + nx)) 0 [2..n]

result :: Int
result = phiSieve (10^6)

------------------------------------------------------------

main :: IO ()
main = time result