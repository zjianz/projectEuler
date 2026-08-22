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

eSeq = concatMap (\x -> [1,2*x,1]) [33,32..1] ++ [2]

continuedrev (p,q) s = (q + p*s, p)

result = digisum . fst $ foldl' continuedrev (1,0) eSeq
    where
        digisum n 
            | n < 10    = n
            | otherwise = n `mod` 10 + digisum (n `div` 10)

------------------------------------------------------------

main :: IO ()
main = time result