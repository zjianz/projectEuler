import System.CPUTime (getCPUTime)
import Data.IntMap ( fromList, IntMap , (!))

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

reachMap :: (Int -> Int) -> IntMap [Int]
reachMap f = fromList (go [] 0 [] 1)
    where
        go ret i mem n
            | fn >= 100*(i+1) = go ((i,mem):ret) (i+1) [] n
            | i > 99          = ret
            | pos < 10        = go ret i mem (n+1)
            | otherwise       = go ret i (pos:mem) (n+1)
            where 
                fn = f n
                pos = fn `mod` 100

tri :: Int -> Int
tri n = n*(n+1) `div` 2
squ :: Int -> Int
squ n = n*n
pen :: Int -> Int
pen n = n*(3*n-1) `div` 2
hex :: Int -> Int
hex n = n*(2*n-1)
hep :: Int -> Int
hep n = n*(5*n-3) `div` 2
oct :: Int -> Int
oct n = n*(3*n-2)

check :: [Maybe a] -> Maybe a
check [] = Nothing
check (p:ps) = p `orElse` check ps

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing that = that
orElse this _       = this

dfs :: Int -> [IntMap [Int]] -> Maybe [Int]
dfs begin reaches = go begin reaches []
    where
        go :: Int -> [IntMap [Int]] -> [IntMap [Int]] -> Maybe [Int]
        go cur [] [] = if cur == begin then Just [cur] else Nothing
        go _ [] _  = Nothing
        go cur (reach:rs) failed = check [(cur:) <$> go new (rs++failed) [] | new <- reach!cur] `orElse` go cur rs (reach:failed)

result :: Maybe Int
result = ((*101) . sum . drop 1) <$> check [dfs ini (map reachMap [tri,squ,pen,hex,hep,oct]) | ini <- [10..99]]

------------------------------------------------------------

main :: IO ()
main = time result