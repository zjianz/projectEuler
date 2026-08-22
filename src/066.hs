import System.CPUTime (getCPUTime)
import Euler
import Data.Foldable (maximumBy)
import Data.Function (on)

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

buildSeq :: Integer -> [Integer]
buildSeq n = (fst seq) ++ (concat . repeat . snd $ seq)
    where seq = continuedFractionOfSqrt n

diophantine :: Integer -> Integer
diophantine n = go 1
    where
        go k
            | p*p - n *q * q == 1 = p
            | otherwise           = go (k+1)
            where
                (p,q) = fromContinuedFraction (take k seq)
        seq = buildSeq n

result :: Integer
result = maximumBy (compare `on` diophantine) . filter (\x -> let y = isqrt x in x /= y * y) $ [1..1000]

------------------------------------------------------------

main :: IO ()
main = time result