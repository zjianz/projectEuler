import System.CPUTime (getCPUTime)
import qualified Data.IntMap.Strict as M

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

-- In Mathematica:
-- f[1] = 1; 
-- f[n_] := f[n] = 1 - Sum[f[Floor@(n/k)], {k, 2, n}]; 
-- Sum[f[Floor@(12000/k)]*(Floor[(k - 1)/2] - Floor[k/3]), {k, 2, 12000}]

-- 抽象一个分块压缩的求和算法
-- blockSum f g gs n = Sum_{d=2}^{n} f([n/d]) * g(d)
-- 其中 gs = Sum_{1}^{#} g
-- 由于 f 需要副作用，因此将状态打包进去

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
result = M.size . snd $ blockSum f fraction fractions 1200000 memo
    where
        memo = M.fromList []
        fraction  k = (k-1) `div` 2 - k `div` 3
        fractions k = --- 手动计算累积函数
            let q = k `div` 3
                r = k `mod` 3 in
                (k `div` 2) * ((k-1) `div` 2) - q * (k+r-1) `div` 2

------------------------------------------------------------

main :: IO ()
main = time result