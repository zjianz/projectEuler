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

limit :: Integer
limit = 1000000

longest ::
    Int ->         -- 最大长度
    Int ->         -- 当前子串长度
    Integer ->     -- 当前子串和
    [Integer] ->   -- 后续质数序列
    Int
longest len index acc (p1:p2:ps)
    | acc > limit = len
    | isPrime acc = longest index (index+2) (acc+p1+p2) ps
    | otherwise   = longest len (index+2) (acc+p1+p2) ps

findbest :: 
    [Integer] ->  -- 当前最佳子串
    [Integer] ->  -- 滑动窗口
    [Integer] ->  -- 后续质数序列
    [Integer]
findbest best group (p:restPrime)
    | acc > limit = best
    | otherwise   = case longest 0 0 acc restPrime of
        0 -> best
        n -> let newbest = group ++ [p] ++ take n restPrime in
                 findbest newbest (tail newbest) (drop n restPrime)
    where acc = sum group + p

result :: Integer
result = sum $ findbest [] [] (tail primes)

------------------------------------------------------------

main :: IO ()
main = time result