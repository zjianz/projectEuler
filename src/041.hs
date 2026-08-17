import System.CPUTime (getCPUTime)
import Data.List (unfoldr)

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

primes :: [Integer]
primes = 2 : sieve [3,5..]
    where sieve (p:ps) = p : sieve (filter (\x -> p * p > x || x `mod` p /= 0) ps)

isPrime :: Integer -> Bool
isPrime n 
    | n > 1 = and [n `mod` p /= 0 | p <- takeWhile (\x -> x*x <= n) primes]
    | otherwise = False

-- Permutation from inc to dec

-- 提取最长递减子串
findPivot :: Ord b => [b] -> Maybe ([b], b, [b])
findPivot ps = go [] ps
    where
        go dec [] = Nothing
        go dec (p:ps)
            | null dec = go [p] ps
            | head dec > p = go (p:dec) ps
            | otherwise    = Just (reverse dec, p, ps)
        
nextPerm :: Ord a => [a] -> Maybe [a]
nextPerm ps = case findPivot ps of
    Nothing -> Nothing
    Just (pre, pivot, rest) -> 
        let (greater, lower) = span (> pivot) pre
            copivot = head lower
            thelower = tail lower
            in Just ((reverse thelower) ++ [pivot] ++ (reverse greater) ++ [copivot] ++ rest)
        

allPerm :: (Num b, Enum b, Ord b) => b -> [b]
allPerm n = map (\xs -> sum (zipWith (*) [10^k | k <- [0..]] xs)) $ [1..n] : unfoldr (\ys -> nextPerm ys >>= \z -> Just (z,z)) [1..n]

result :: Integer
result = head $ filter isPrime (concatMap allPerm [7,4,1])

------------------------------------------------------------

main :: IO ()
main = time result