import System.CPUTime (getCPUTime)
import Math.NumberTheory.Primes (UniqueFactorisation(isPrime), Prime)

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

isprime p = case isPrime p of
    Nothing -> False
    _       -> True

oddComposite :: [Integer]
oddComposite = filter (not . isprime) [3,5..]

result :: Integer
result = head $ filter goldbach oddComposite
    where
        goldbach n = all (\x -> (not . isprime) (n - 2*x*x)) (takeWhile (\x -> 2*x*x < n) [1..])

------------------------------------------------------------

main :: IO ()
main = time result