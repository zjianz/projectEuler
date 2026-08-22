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

-- (a,b,c,d -> (b√a + c) / d)
type Qsqrt = (Int, Int, Int, Int)

qFloor :: Qsqrt -> (Qsqrt, Int)
qFloor (a,b,c,d) = go [0..]
    where
        go (k:ks)
            | b > 0 && c >= kd                     = go ks
            | b > 0 && a * b * b > (kd-c) * (kd-c) = go ks
            | b < 0 && a * b * b < (c-kd) * (c-kd) = go ks
            | otherwise                            = ((a,b,(c-kd+d),d), k-1) 
            where kd = k * d

simplify :: Qsqrt -> Qsqrt
simplify (a,b,c,d) = let g = foldl1 gcd [b,c,d] in (a,b`div`g,c`div`g,d`div`g)

inver :: Qsqrt -> Qsqrt
inver (a,b,c,d) = simplify (a,d*b,(-1)*d*c,a*b*b-c*c)

chain :: Int -> Int
chain n = go [] (n,1,0,1)
    where
        go cache q = go' cache (inver . fst . qFloor $ q) 1
            where
                go' [] nq _ = go (nq:cache) nq
                go' (q':qs) nq k
                    | q' == nq  = k
                    | otherwise = go' qs nq (k+1) 

result :: Int
result = go [1..10000] [1..] 0
    where
        go [] _ c = c
        go (u:us) (v:vs) c
            | u == v*v        = go us vs c
            | odd . chain $ u = go us (v:vs) (c+1)
            | otherwise       = go us (v:vs) c

------------------------------------------------------------

main :: IO ()
main = time result