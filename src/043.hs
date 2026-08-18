import System.CPUTime (getCPUTime)

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

fromDigit :: [Int] -> Int
fromDigit ps = sum (zipWith (*) (reverse ps) [10^k | k <- [0..]])

seed :: [[Int]]
seed = [toDigit x | x <- [17,34..1000], let [a,b,c] = toDigit x, a /= b, a /= c, b /= c]
    where toDigit n = [n `div` 100, (n `div` 10) `mod` 10, n `mod` 10]

getChild :: [Int] -> Int -> [[Int]]
getChild node factor = [ x:node | x <- [0..9], not (x `elem` node), (fromDigit (x:(take 2 node))) `mod` factor == 0]

result :: Int
result = sum $ map fromDigit $ filter ((/=0) . head) $ foldl' (\ys -> \x -> concatMap (flip getChild x) ys) seed [13,11,7,5,3,2,1]

------------------------------------------------------------

main :: IO ()
main = time result