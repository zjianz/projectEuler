module Euler (primes, isPrime) where

primes :: [Integer]
primes = 2 : sieve [3,5..]
    where
        sieve [] = []
        sieve (p:ps) = p : sieve (filter (\x -> p * p > x || x `mod` p /= 0) ps)

isPrime :: Integer -> Bool
isPrime n 
    | n > 1 = and [n `mod` p /= 0 | p <- takeWhile (\x -> x*x <= n) primes]
    | otherwise = False
