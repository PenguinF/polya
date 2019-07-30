module Eutherion.Combinatorics (

       binom,
       multinom,
       choose,

       CayleyTable,
       ctSize,
       ctIdentityElement,
       ctOperationTable,
       ctInverseTable

       ) where

import Data.Array
import Data.List

import Eutherion.Utilities

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

-- Enumerates all combinations to choose n items out of a set of k distinct elements.
-- Example: to enumerate all ways of choosing 3 characters out of 'x' and 'y':
--
-- > choose "xy" 3
-- [(1,[(3,'x'),(0,'y')]),
--  (3,[(2,'x'),(1,'y')]),
--  (3,[(1,'x'),(2,'y')]),
--  (1,[(0,'x'),(3,'y')])]
--
-- I.e. 1 way to choose 3 'x', 3 ways of choosing either 2 'x' and 1 'y' or 1 'x' and 2 'y', and 1 way to choose 3 'y'.
choose :: [a] -> Integer -> [(Integer, [(Integer, a)])]
choose items n =
    let k = toInteger $ length items
    in  [(multinom n distinctSum, zip distinctSum items) | distinctSum <- reverse $ genDistinctSums n k]
    where
        -- Generates all distinct combinations of k integers which add up to a total of n.
        genDistinctSums :: (Eq a, Enum a, Num a, Eq b, Num b) => a -> b -> [[a]]
        genDistinctSums n k =
            case (n, k) of
                (0, 0) -> [[]]  -- Sum matches total exactly, accept, so yield an empty (valid) solution.
                (_, 0) -> []    -- Sum does not match total, reject, so yield nothing at all.
                (n, k) -> [i : xs | i <- [0..n], xs <- genDistinctSums (n - i) (k - 1)]

-- Represents a finite group, with these identities:
-- (n == ctSize, t == ctOperationTable, v == ctInverseTable, i == ctIdentityElement)
--
-- 0 <= t[x, y] < n  for all 0 <= x,y < n  (binary algebra)
-- t[t[x, y], z] = t[x, t[y, z]]           (associativity)
-- t[i, y] == y                            (monoid I)
-- t[x, i] == x                            (monoid II)
-- t[x, v[x]] = i = t[v[x], x]             (inverse law)
data CayleyTable = CayleyTable {
        ctSize :: Int,
        ctIdentityElement :: Int,
        ctOperationTable :: (Array Int (Array Int Int)),
        ctInverseTable :: (Array Int Int)
    }

-- Shows a Cayley table given a function which yields (short) names for each index in the table.
showCayleyTable :: String -> (Int -> String) -> CayleyTable -> String
showCayleyTable idElemString f (CayleyTable n identity multTable inverseTable) =
    intercalate "\n" $ map leftMargin $ concat
    [
        [header],
        [headerBorder],
        [tableEntryLine i | i <- [0..n - 1]]
    ]
    where
        displayElem e
            | e == identity = idElemString
            | otherwise     = f e

        maxElemLength = maximum [length $ displayElem i | i <- [0..n - 1]]

        leftMargin line = "  " ++ line

        rAlign          = padLeft ' '
        rAlignElemWidth = rAlign maxElemLength
        rAlignElem      = rAlignElemWidth . displayElem

        -- header/headerBorder/tableEntryLine i should have parallel implementations.
        header =
            replicate maxElemLength ' '
            ++ " |"
            ++ concat [' ' : rAlignElem i | i <- [0..n - 1]]
            ++ " | "
            ++ rAlignElemWidth "~"

        headerBorder =
            replicate maxElemLength '-'
            ++ "-+"
            ++ concat ['-' : replicate maxElemLength '-' | i <- [0..n - 1]]
            ++ "-+-"
            ++ replicate maxElemLength '-'
            ++ "-"

        tableEntryLine i =
            rAlignElem i
            ++ " |"
            ++ concat [' ' : (rAlignElem (multTable ! i ! j)) | j <- [0..n - 1]]
            ++ " | "
            ++ rAlignElem (inverseTable ! i)

instance Show CayleyTable where
    show = showCayleyTable "1" (formatAsNumber ['a'..'z'] . toInteger)
