module Eutherion.Combinatorics (

       binom

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
