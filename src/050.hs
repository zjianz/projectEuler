import System.CPUTime (getCPUTime)
import Euler (primes, isPrime)

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

limit :: Integer
limit = 1000000

walker n acc (p1:p2:ps)
    | acc > limit = []
    | isPrime acc = (walker (n+2) (acc+p1+p2) ps) ++ [n]
    | otherwise   = walker (n+2) (acc+p1+p2) ps

findbest best group (p:restPrime)
    | acc > limit = best
    | otherwise   = case walker 0 acc restPrime of
        []    -> best
        (n:_) -> let newbest = group ++ [p] ++ take n restPrime in
                     findbest newbest (tail newbest) (drop n restPrime)
    where acc = sum group + p

result :: Integer
result = sum $ findbest [] [] primes

------------------------------------------------------------

main :: IO ()
main = time result