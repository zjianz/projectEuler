import System.CPUTime (getCPUTime)
import Data.List (scanl')

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

inv_binary_digits :: Integral t => t -> [t]
inv_binary_digits 0 = [0]
inv_binary_digits 1 = [1]
inv_binary_digits n = k : (inv_binary_digits (n `div` 2))
    where k = n `mod` 2

palidromes_even :: [Int]
palidromes_even = [ evenfy x | x <- [1..]]
    where evenfy x = read $ (\str -> str ++ (reverse str)) (show x)

palidromes_odd :: [Int]
palidromes_odd = [ oddfy x | x <- [1..]]
    where oddfy x = read $ (\str -> (init str) ++ (reverse str)) (show x)

limit :: Int
limit = 1000000

result :: Int
result = foldl' (+) 0 $ filter test ((takeWhile (<limit) palidromes_even) ++ (takeWhile (<limit) palidromes_odd))
    where test x = (\lst -> lst == reverse lst) (inv_binary_digits x)
------------------------------------------------------------

main :: IO ()
main = time result