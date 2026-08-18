import System.CPUTime (getCPUTime)
import Euler (primes, isPrime)

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

prime4 = dropWhile (<1000) $ takeWhile (<10000) primes

equiv [] qs = null qs
equiv (p:ps) qs = case rest of
    [] -> False
    (q:rrest) -> equiv ps (pre++rrest)
    where
        (pre,rest) = span (/=p) qs

candidates = go prime4
    where
        go [] = []
        go (p:ps) = scan p ps ++ go ps
        scan p ps = [ [p,q,r] | q <- ps, equiv (show p) (show q),let r = 2*q-p, equiv (show p) (show r), isPrime r]

result :: Integer
result = read $ head $ filter (/="148748178147") $ map (concatMap show) candidates

------------------------------------------------------------

main :: IO ()
main = time result