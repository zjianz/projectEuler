import System.CPUTime (getCPUTime)
import Math.NumberTheory.Primes (UniqueFactorisation(factorise))

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

factorNum :: [(Int, Int)]
factorNum = map (\x -> (x, length $ factorise x)) [1..]


consecutive :: forall a. (a -> Bool) -> Int -> [a] -> [a]
consecutive condition n sequ = reverse $ go n sequ []
    where 
        go :: Int -> [a] -> [a] -> [a]
        go 0 _ mem      = mem
        go _ [] _       = undefined
        go k (p:ps) mem
            | (condition p) = go (k-1) ps (p:mem)
            | otherwise     = go n ps []

result :: Int
result = fst . head $ consecutive (\(_,x) -> x==4) 4 factorNum 

------------------------------------------------------------

main :: IO ()
main = time result