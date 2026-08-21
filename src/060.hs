import System.CPUTime (getCPUTime)
import Euler (isPrime, primes)

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
oprimes = 3 : drop 3 primes

befriend :: Integer -> Integer -> Bool
befriend p q = (p+q) `mod` 3 /= 0 && (isPrime . read $ show p ++ show q) && (isPrime . read $ show q ++ show p)

befriendWith :: Foldable t => Integer -> t Integer -> Bool
befriendWith p = all (befriend p)

dfs :: [Integer]
    -> Int
    -> Integer
    -> [Integer]
    -> (Integer, [Integer])
    -> (Integer, [Integer])
dfs _ _ _ [] (best, bestNode) = (best, bestNode)
dfs node rest total cand@(c:cs) (best, bestNode)
    | total + sum rcand >= best = (best, bestNode)
    | rest == 0                 = (total, node)
    | length rcand < rest       = (best, bestNode)
    | not (befriendWith c node) = dfs node rest total cs (best, bestNode)
    | otherwise                 = dfs node rest total cs (dfs (c:node) (rest-1) (total+c) cs (best, bestNode))
    where
        rcand = take rest cand

result :: Integer
result = fst $ dfs [] 5 0 oprimes (upperbound,[])

------------------------------------------------------------

main :: IO ()
main = time result