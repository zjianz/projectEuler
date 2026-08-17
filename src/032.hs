import System.CPUTime (getCPUTime)
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

condProd :: Int -> Int -> Int
condProd a b
    | p >= 10000 = 0
    | all (`elem` digits) ['1'..'9'] = p
    | otherwise  = 0
    where
        p      = a * b
        digits = concatMap show [a,b,p] 

result :: Int
result = sum [condProd a b | a <- [10..99], b <- [100..999]]

------------------------------------------------------------

main :: IO ()
main = time result