import System.CPUTime (getCPUTime)
import qualified Data.HashTable.ST.Basic as H
import Control.Monad.ST (runST, ST)

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

-- 假设我们排查的范围内，n^3中同一个数字不会重复出现7次以上
security :: Integer
security = 7
fingerPrint :: Integer -> Integer
fingerPrint 0 = 0
fingerPrint n = security^(n`mod`10) + fingerPrint (n`div`10)

result = runST $ do
    memory <- H.new :: ST s (H.HashTable s Integer (Integer,Int))
    go memory 1
    where 
        insert :: Integer -> Maybe (Integer,Int) -> (Maybe (Integer,Int), (Integer,Int))
        insert val Nothing   = (Just (val,1),(val,1))
        insert _ (Just (v,n)) = (Just (v,n+1) , (v,n+1))
        -- go :: (H.HashTable s Integer (Integer,Int)) -> Integer -> Integer
        go m x = do
            let val = (x^3)
            let fp = fingerPrint val
            updated <- H.mutate m fp (insert val)
            if (snd updated >= 5) then return $ fst updated else go m (x+1)

------------------------------------------------------------

main :: IO ()
main = time result