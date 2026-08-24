import System.CPUTime (getCPUTime)
import Data.IntMap.Strict as M

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

-- Same as 73

blockSum :: (Int -> s -> (Int, s)) -> (Int->Int) -> (Int->Int) -> Int -> s -> (Int, s)
blockSum f g gs n s = go1 s 2 0
    where
        go1 s1 d acc
            | k' == k   = go2 s1 k acc
            | otherwise = go1 s1' (d+1) (acc + fk * g d)
            where
                k  = n `div` d
                k' = n `div` (d+1)
                (fk, s1') = f k s1
        go2 s2 k acc
            | k == 0    = (acc, s2)
            | otherwise = go2 s2' (k-1) (acc + fk * (gs d - gs d'))
            where
                d  = n `div` k
                d' = n `div` (k+1)
                (fk, s2') = f k s2

type Memo = M.IntMap Int

f :: Int -> Memo -> (Int, Memo)
f 1 memo = (1, memo)
f n memo = case M.lookup n memo of
    Just x -> (x, memo)
    Nothing -> let
        (x, memo') = blockSum f (const 1) id n memo
        in
            (1-x, M.insert n (1-x) memo')

result :: Int
result = fst $ blockSum f fraction fractions 1000000 memo
    where
        memo = M.fromList []
        fraction  k = (k-1)
        fractions k = (k-1) * k `div` 2

------------------------------------------------------------

main :: IO ()
main = time result