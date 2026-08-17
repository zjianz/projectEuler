import System.CPUTime (getCPUTime)
import Data.IntMap ( (!), fromList, IntMap )
import Data.List (scanl')

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

fact :: IntMap Int
fact = fromList $ zip [0..9] (scanl' (*) 1 [1..9])

result :: Int
result = sum [num |
    a <- [0..9],
    b <- [0..9],
    c <- [0..9],
    d <- [0..9],
    e <- [0..9],
    f <- [0..9],
    g <- [0..9],
    let num = a + 10*(b+10*(c+10*(d+10*(e+10*(f+10*g))))),
    num == (fact ! a) + (fact ! b) + (fact ! c) + (fact ! d) + (fact ! e) + (fact ! f) + (fact ! g)
    ] - 3

------------------------------------------------------------

main :: IO ()
main = time result