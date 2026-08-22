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

eSeq :: [Integer]
eSeq = 2 : concatMap (\x -> [1,2*x,1]) [1..]

result :: Integer
result = digisum . fst . fromContinuedFraction . take 100 $ eSeq
    where
        digisum n 
            | n < 10    = n
            | otherwise = n `mod` 10 + digisum (n `div` 10)

------------------------------------------------------------

main :: IO ()
main = time result