import System.CPUTime (getCPUTime)
import Data.Foldable (find)

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

pell :: (Integer, Integer) -> (Integer, Integer)
pell (x, y) = (x+2*y, x+y)

solve :: (Integer, Integer) -> (Integer, Integer)
solve (x,y)
    | x^2 - 2*y^2 == -1 = let n = x - y in (n * x + 1, 2 * n * y + 1)
    | x^2 - 2*y^2 ==  1 = let n = 2*y - x in (n * y + 1, n * x + 1)
    | otherwise = (0,0)

limit :: Integer
limit = 10^12

result :: Integer
result = fst . head . dropWhile ((< limit) . snd) . map solve . iterate pell $ (1,1)

------------------------------------------------------------

main :: IO ()
main = time result