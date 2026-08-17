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

matches :: [Integer]
matches = [ a*b*c | 
    m <- [2..], 
    n <- [1..(m-1)], 
    let a = m^2 - n^2,
    let b = 2*m*n,
    let c = m^2 + n^2,
    a+b+c == 1000]

result :: Integer
result = case matches of
    (x:_) -> x
    []    -> 0
    

------------------------------------------------------------

main :: IO ()
main = time result