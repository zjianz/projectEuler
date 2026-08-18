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

-- 最大公约数（欧几里得算法）
gcd' :: Int -> Int -> Int
gcd' a 0 = abs a
gcd' a b = gcd' b (a `mod` b)

-- 判断是否与10互质
coprimeTo10 :: Int -> Bool
coprimeTo10 k = gcd' k 10 == 1

-- 质因数分解（简单试除法，返回 [(素因子, 指数)]）
factorize :: Int -> [(Int, Int)]
factorize n = go n 2
  where
    go 1 _ = []
    go m p
      | p * p > m = [(m, 1)]
      | m `mod` p == 0 =
          let (cnt, rest) = countFactors m p 0
          in (p, cnt) : go rest (p + 1)
      | otherwise = go m (p + 1)
    countFactors m p c
      | m `mod` p == 0 = countFactors (m `div` p) p (c + 1)
      | otherwise = (c, m)

-- 快速幂取模：计算 (a^e) `mod` m
powMod :: Int -> Int -> Int -> Int
powMod a e m = go a e 1
  where
    go _ 0 acc = acc
    go base exp acc
      | exp `mod` 2 == 1 = go (base * base `mod` m) (exp `div` 2) (acc * base `mod` m)
      | otherwise        = go (base * base `mod` m) (exp `div` 2) acc

-- 计算 ord_m(a)，即 a 模 m 的阶（要求 gcd(a,m)=1）
ordMod :: Int -> Int -> Int
ordMod a m
  | gcd' a m /= 1 = 0
  | otherwise = 
      let phi = eulerPhi m
          -- 获取 phi 的所有因子（去重）
          divisors = factorDivisors phi
      in minimum [d | d <- divisors, powMod a d m == 1]
  where
    -- 欧拉函数（简单实现）
    eulerPhi n = foldl (\acc (p,_) -> acc `div` p * (p-1)) n (factorize n)
    -- 生成所有因子（不排序）
    factorDivisors n = go [1] (factorize n)
      where
        go acc [] = acc
        go acc ((p,e):rest) =
          let new = [acc' * p^exp | acc' <- acc, exp <- [0..e]]
          in go new rest

-- 计算 1/k 的循环节长度（优化版）
rankOptimized :: Int -> Int
rankOptimized k
  | k <= 0          = 0
  | k == 1          = 0
  | not (coprimeTo10 k) = 0
  | otherwise = 
      -- 分解 k 为素因子幂的乘积
      let factors = factorize k
          -- 对每个 p^e 计算 ord_{p^e}(10)
          ords = [ordMod 10 (p^e) | (p,e) <- factors]
      in foldl lcm 1 ords  -- 取最小公倍数

-- 主程序：寻找 1..limit 中使循环节长度最大的 k
limit :: Int
limit = 1000

result :: Int
result = snd $ foldl' cmp (0,0) $ 
         [(rankOptimized k, k) | k <- [1..limit], coprimeTo10 k]
  where
    cmp (!r1,!i1) (!r2,!i2)
      | r1 > r2   = (r1,i1)
      | otherwise = (r2,i2)


------------------------------------------------------------

main :: IO ()
main = time result