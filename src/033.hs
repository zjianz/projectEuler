import System.CPUTime (getCPUTime)
import Data.Ratio

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


result :: Int
result = denominator $ product 
    [ a % c
    | a <- [1..9], b <- [1..9], c <- [1..9]
    , (10*a + b) % (10*b + c) == a % c
    , a < c
    ]

------------------------------------------------------------

main :: IO ()
main = time result