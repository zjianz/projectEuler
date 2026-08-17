import System.CPUTime (getCPUTime)
import Data.Array.ST
    ( freeze, readArray, writeArray, MArray(newArray), STUArray )
import Data.Array.Unboxed (UArray, (!))
import Control.Monad.ST ( ST, runST )
import Control.Monad ( when, forM_ )

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

sieveTo :: Int -> UArray Int Bool
sieveTo n = runST $ do
    let upperbound = n
    numbers <- newArray (1,upperbound) True :: ST s (STUArray s Int Bool)
    forM_ [2..upperbound] $ \i -> do
        flag <- readArray numbers i
        when flag $ do
            forM_ [i*i,i*i+i..upperbound] $ \j -> writeArray numbers j False
    freeze numbers

primeQArr :: UArray Int Bool
primeQArr = sieveTo 1000000

primeQ :: Int -> Bool
primeQ n 
    | n > 1 = primeQArr ! n
    | otherwise = False


circularQ :: [Char] -> Bool
circularQ [] = False
circularQ seqs = go seqs (length seqs)
    where
        go [] _     = False
        go _ 0      = True
        go (s:ss) n = 
            let num = read (s:ss) :: Int
            in  primeQ num && go (ss++[s]) (n-1)

result :: Int
result = length [x | x :: Int <- [2..1000000], circularQ (show x)]

------------------------------------------------------------

main :: IO ()
main = time result