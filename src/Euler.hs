module Euler (primes, isPrime, isqrt, continuedFractionOfSqrt, fromContinuedFraction) where
import Data.Foldable (Foldable(foldr'))

primes :: [Integer]
primes = 2 : sieve [3,5..]
    where
        sieve [] = []
        sieve (p:ps) = p : sieve (filter (\x -> p * p > x || x `mod` p /= 0) ps)

isPrime :: Integer -> Bool
isPrime n 
    | n > 1 = and [n `mod` p /= 0 | p <- takeWhile (\x -> x*x <= n) primes]
    | otherwise = False


isqrt :: Integer -> Integer
isqrt n
  | n < 2     = n
  | otherwise = go n (n `div` 2)
  where
    go x y
      | y < x     = go y ((y + n `div` y) `div` 2)
      | otherwise = x

continuedFractionOfSqrt :: Integer -> ([Integer], [Integer])
continuedFractionOfSqrt n = go (0, (0, 1, 1)) []
    where
        next (_, (a, b, c)) = (digit, (na, nb, nc))
            where
                digit = (isqrt (b*b*n) + a) `div` c
                nc'    = b * b * n - (a-digit*c) * (a-digit*c)
                nb'    = c * b
                na'    = (-1) * c * (a-digit*c)
                g      = gcd na' $ gcd nb' nc'
                (na,nb,nc) = (na' `div` g, nb' `div` g, nc' `div` g)
        go qRoot cache = case span ((/= (snd newRoot)) . snd) cache of
            (_, []) -> go newRoot (newRoot:cache)
            (c1, c2) -> (map fst c2, map fst . reverse . (newRoot:) $ c1)
            where
                newRoot = next qRoot
            
fromContinuedFraction :: (Integral a) => [a] -> (a,a)
fromContinuedFraction = foldr' next (1,0)
    where
        next s (p,q) = (q+p*s,p)