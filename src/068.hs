import System.CPUTime (getCPUTime)
import Data.List (sort)

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


shift :: [a] -> [a]
shift r = last r : init r

loops :: Integer -> [[Integer]]
loops n = concatMap (\k -> go [k+1..2*n] [k] []) [1..n+1]
    where
        go [] ring _ = if condition ring then [ring] else []
        go cand@(c:cs) ring failed
            | length ring + length cand < fromInteger n = []
            | length ring < fromInteger n = (go (cs++failed) (c:ring) []) ++ (go cs ring (c:failed))
            | condition ring    = [ring]
            | otherwise       = []
        condition ring = let s = sum ring in
            s `mod` n == 0 && sort (ring ++ map (\(x,y) -> 2*n+1 + (s `div` n) - x - y) (zip ring (shift ring))) == [1..2*n]

parseLoop :: [Integer] -> Integer
parseLoop loop = read . concatMap show . minimum . map decode . take n . iterate shift $ loop
    where 
        n = length loop
        s = (sum loop) `div` (toInteger n) + 2*(toInteger n) + 1
        decode l = concat $ zipWith (\x y -> [s - x - y, y, x]) l (shift l)

result :: Integer
result = maximum . filter (<10^16) . map parseLoop . concatMap (\x -> [x, reverse x]) $ loops 5

------------------------------------------------------------

main :: IO ()
main = time result