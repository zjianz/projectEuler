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

isPalindrome :: Integer -> Bool
isPalindrome n = ndigits == reverse ndigits
    where ndigits = show n --- wtf!

result :: Integer
result = go 999 999 0
    where go x y res
            | x * x < res = res
            | y <= 100    = go (x-1) (x-1) newres
            | otherwise   = go x (y-1) newres
            where
                p      = x*y 
                newres =
                    if isPalindrome (p)
                    then max res (p)
                    else res

------------------------------------------------------------

main :: IO ()
main = time result