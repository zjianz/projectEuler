import System.CPUTime (getCPUTime)
import Euler (primes)
import Math.NumberTheory.Primes.Testing (isPrime)

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

upperbound :: Integer
upperbound = 30000

oprimes :: [Integer]
oprimes = 3 : (takeWhile (<upperbound) $ dropWhile (<=5) $ primes)

digitShift :: Integer -> Integer
digitShift n
    | n < 10       = 10
    | n < 100      = 100
    | n < 1000     = 1000
    | n < 10000    = 10000
    | n < 100000   = 100000
    | n < 1000000  = 1000000
    | n < 10000000 = 10000000
    | otherwise    = 100000000


concatNum :: Integer -> Integer -> Integer
concatNum p q = p * digitShift q + q

befriend :: Integer -> Integer -> Bool
befriend p q = (p+q) `mod` 3 /= 0 && isPrime pq && isPrime qp
    where
        pq = concatNum p q
        qp = concatNum q p


dfs :: [Integer]
    -> Int
    -> Integer
    -> [Integer]
    -> (Integer, [Integer])
    -> (Integer, [Integer])
dfs node 0 total _ _            = (total, node)
dfs _ _ _ [] (best, bestNode)   = (best, bestNode)
dfs node rest total cand@(c:cs) (best, bestNode)
    | length rcand < rest       = (best, bestNode)
    | total + sum rcand >= best = (best, bestNode)
    | otherwise                 = dfs node rest total cs (dfs (c:node) (rest-1) (total+c) (filter (befriend c) cs) (best, bestNode))
    where
        rcand = take rest cand

result :: Integer
result = fst $ dfs [] 5 0 oprimes (upperbound,[])

------------------------------------------------------------

main :: IO ()
main = time result