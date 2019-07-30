module Eutherion.Combinatorics (

       binom,
       multinom

       ) where

-- Computes the binomial coefficient 'n choose k'.
binom :: Integer -> Integer -> Integer
binom n k
    | n < 0          = error ("binom n k undefined for n < 0, n == " ++ show n)
    | k < 0 || n < k = error ("binom n k undefined for k < 0 or n < k, n == " ++ show n ++ ", k == " ++ show k)
    | n `div` 2 < k  = binomChecked n (n - k)
    | otherwise      = binomChecked n k
    where
        binomChecked n k =
            case k of
                0 -> 1
                k -> n * (binomChecked (n - 1) (k - 1)) `div` k

-- Computes the multinomial coefficient of n choose k[i] over a range of i.
-- multinom n [1 | i <- [1..n]] == n!
-- See also: https://en.wikipedia.org/wiki/Multinomial_theorem
multinom :: Integer -> [Integer] -> Integer
multinom n ks =
    case ks of
        []   -> 1
        k:ks -> multinom (n - k) ks * binom n k
