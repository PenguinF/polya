module Eutherion.Combinatorics (

       binom,
       multinom,
       choose,

       CayleyTable,
       ctSize,
       ctIdentityElement,
       ctOperationTable,
       ctInverseTable,
       buildCayleyTable,
       buildCayleyTableOptimistic

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
    in  map coefficientAndDistribution $ genDistinctSums n k
    where
        coefficientAndDistribution distinctSum = (multinom n distinctSum, zip distinctSum items)

        -- Generates all distinct combinations of k integers which add up to a total of n.
        genDistinctSums n k =
            case (n, k) of
                (0, 0) -> [[]]  -- Sum matches total exactly, accept, so yield an empty (valid) solution.
                (_, 0) -> []    -- Sum does not match total, reject, so yield nothing at all.
                (n, k) -> [i : xs | i <- decreasingRange n 0, xs <- genDistinctSums (n - i) (k - 1)]

        decreasingRange max min
            | max < min = []
            | otherwise = max : decreasingRange (max - 1) min


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

-- Builds a Cayley table of a finite group, if it is indeed a group. Yields a list of errors otherwise.
buildCayleyTable :: Int -> Int -> Array Int (Array Int Int) -> Array Int Int -> Either [String] CayleyTable
buildCayleyTable size identityElement operationTable inverseTable =
    if errors2 /= [] then Left errors2 else Right (CayleyTable size identityElement operationTable inverseTable)
    where
        errors1 = case dimensionErrors of
            [] -> closedErrors
            _  -> dimensionErrors

        errors2 = case errors1 of
            [] -> concat [
                      associativityErrors,
                      identityErrors,
                      inverseErrors]
            _  -> errors1

        range (min, max) = [min..max]

        analyzeBounds (min, max) errorPrefix =
            conditionalElem (min /= 0 || max + 1 /= size)
                            (errorPrefix ++ " has bounds " ++ show min ++ ".." ++ show max ++ ", but expected 0.." ++ show (size - 1))

        dimensionErrors =
            concat [
                conditionalElem (size <= 0) "Table has zero or negative size",
                analyzeBounds (bounds operationTable) "Operation table",
                concat [analyzeBounds (bounds (operationTable ! i)) ("Operation table [" ++ show i ++ "]") | i <- range $ bounds operationTable],
                analyzeBounds (bounds inverseTable) "Inverse table"]

        analyzeClosed x errorPrefix =
            conditionalElem (x < 0 || size <= x)
                            (errorPrefix ++ " is not within the finite group, bounds 0.." ++ show (size - 1))

        analyzeElementClosed x y =
            analyzeClosed opResult (show x ++ " . " ++ show y ++ " = " ++ show opResult)
            where
                opResult = operationTable ! x ! y

        analyzeInverseClosed x =
            analyzeClosed inverseResult ("~" ++ show x ++ " = " ++ show inverseResult)
            where
                inverseResult = inverseTable ! x

        closedErrors =
            concat [
                analyzeClosed identityElement ("Identity element " ++ show identityElement),
                concat [analyzeElementClosed i j | i <- [0..size - 1], j <- [0..size - 1]],
                concat [analyzeInverseClosed i | i <- [0..size - 1]]]

        associativityError i j k =
            conditionalElem (leftResult /= rightResult)
                            ("(" ++ show i ++ " . " ++ show j ++ ") . " ++ show k ++ " = " ++ show leftResult ++ " <> " ++
                             show rightResult ++ " = " ++ show i ++ " . (" ++ show j ++ " . " ++ show k ++ ")")
            where
                leftResult  = operationTable ! (operationTable ! i ! j) ! k
                rightResult = operationTable ! i ! (operationTable ! j ! k)

        associativityErrors =
            concat [associativityError i j k | i <- [0..size - 1], j <- [0..size - 1], k <- [0..size - 1]]

        identityError i =
            conditionalElem (i /= opResult)
                            ("I . " ++ show i ++ " = " ++ show opResult ++ " <> " ++ show i)
            where
                opResult = operationTable ! identityElement ! i

        identityErrors =
            concat [identityError i | i <- [0..size - 1]]

        inverseError i =
            conditionalElem (identityElement /= invResult)
                            ("~" ++ show i ++ " . " ++ show i ++ " = " ++ show invResult ++ " <> I")
            where
                invResult = operationTable ! (inverseTable ! i) ! i

        inverseErrors =
            concat [inverseError i | i <- [0..size - 1]]

-- Like buildCayleyTable, builds a Cayley table of a finite group. Throws if it's not a group.
buildCayleyTableOptimistic :: Int -> Int -> Array Int (Array Int Int) -> Array Int Int -> CayleyTable
buildCayleyTableOptimistic n identity multTable inverseTable =
    let maybeTable = buildCayleyTable n identity multTable inverseTable
    in  case maybeTable of
        Left errors  -> error $ intercalate "\n" ("Not a finite group:" : errors)
        Right cTable -> cTable
