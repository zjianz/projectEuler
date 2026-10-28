import System.CPUTime (getCPUTime)
import Data.List (tails)

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

predict :: [Integer] -> Integer
predict [] = 0
predict [n] = n
predict ps@(p:_) = p + predict (zipWith (-) ps (drop 1 ps))  

polygen :: [Integer]
polygen = [sum $ zipWith (*) (cycle [1,-1]) (map (x^) [0..10]) | x <- [1..10]]

result :: Integer
result = sum . map predict . tails . reverse $ polygen

------------------------------------------------------------

main :: IO ()
main = time result