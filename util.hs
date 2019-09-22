module Eutherion.Utilities (

       fst3,
       snd3,
       thd3,
       formatAsNumber,
       exponentCharacterLookup,
       expToNumber,
       padLeft,
       padRight,
       conditionalElem,
       findArrayIndex,
       findArrayIndexBy,
       joinList,
       groupAndSort

       ) where

import Data.Array

fst3 :: (a, b, c) -> a
fst3 (x, y, z) = x

snd3 :: (a, b, c) -> b
snd3 (x, y, z) = y

thd3 :: (a, b, c) -> c
thd3 (x, y, z) = z

-- Formats an integer value using a custom digit character lookup.
-- A dash ('-') is used for a minus sign.
-- The digit character lookup must have two or more characters.
--
-- Format a binary value:
-- > formatAsNumber "01" 13
-- "1101"
--
-- Format as hex value, C-style:
-- > "0x" ++ formatAsNumber "0123456789ABCDEF" 91
-- "0x5B"
formatAsNumber :: [Char] -> Integer -> String
formatAsNumber lookup n
    | base == 0 = error "Empty digit character lookup argument in formatAsNumber"
    | base == 1 = error "Unary character lookup argument in formatAsNumber"
    | n < 0     = '-' : (fmtPositiveNumber lookup (-n))
    | otherwise = fmtPositiveNumber lookup n
    where
        base = toInteger $ length lookup
        fmtPositiveNumber lookup n =
            let (d, m)    = (n `div` base, n `mod` base)
                lastDigit = [lookup !! fromInteger m]
            in  if (d == 0)
                    then lastDigit
                    else fmtPositiveNumber lookup d ++ lastDigit

-- For showing exponents. See also formatAsNumber.
exponentCharacterLookup :: String
exponentCharacterLookup = "⁰¹²³⁴⁵⁶⁷⁸⁹"

-- Converts a superscript (exponent) digit character to an integer value.
expToNumber :: Num a => Char -> (Bool, a)
expToNumber c = etnHelper 0 exponentCharacterLookup c
    where
        etnHelper n [] c = (False, error "Not an exponent character.")
        etnHelper n (x:xs) c
            | c == x    = (True, n)
            | otherwise = etnHelper (n + 1) xs c

-- Pads a list on the left with an element up to a maximum total length.
padLeft :: a -> Int -> [a] -> [a]
padLeft c n s = replicate (n - length s) c ++ s

-- Pads a list on the right with an element up to a maximum total length.
padRight :: a -> Int -> [a] -> [a]
padRight _ 0 s     = s
padRight c n []    = replicate n c
padRight c n (x:s) = x : padRight c (n - 1) s

-- Returns a list containing one given value or an empty list depending on a condition.
conditionalElem :: Bool -> a -> [a]
conditionalElem b c
    | b         = [c]
    | otherwise = []

-- Finds the index of an element of an array.
findArrayIndex :: (Ix a, Eq t) => t -> Array a t -> Maybe a
findArrayIndex needle = findArrayIndexBy (==needle)

-- Finds the index of an element of an array.
findArrayIndexBy :: Ix a => (t -> Bool) -> Array a t -> Maybe a
findArrayIndexBy predicate = findArrayIndexBy' predicate . assocs
    where
        findArrayIndexBy' predicate []                       = Nothing
        findArrayIndexBy' predicate ((i, needle) : hayStack) = if predicate needle
                                                               then Just i
                                                               else findArrayIndexBy' predicate hayStack

-- Joins a list of elements with an infix between two entries.
joinList :: (a -> a -> [b]) -> (a -> [b]) -> [a] -> [b]
joinList infixFn showFn elements =
    case elements of
        []   -> []
        x:xs -> joinListInner infixFn showFn x xs
    where
        joinListInner infixFn showFn x1 elements =
            case elements of
                []    -> showFn x1
                x2:xs -> showFn x1 ++ infixFn x1 x2 ++ joinListInner infixFn showFn x2 xs

-- Allows quick-sorting with a custom function to deal with groups of equal elements.
groupAndSort :: (a -> [a] -> [b])     -- Function applied on a pivot element and all other elements from the list equal to it,
                                      -- which returns a list of result elements to add between smaller and greater elements.
             -> (a -> a -> Ordering)  -- Function to compare two elements.
             -> [a]                   -- List of elements to group.
             -> [b]                   -- Returns the sorted list of grouped elements.
groupAndSort _ _ [] = []
groupAndSort fn cmp (pivot:xs) =
    let (smaller, equal, greater) = pivotForQSort cmp pivot xs
    in  groupAndSort fn cmp smaller
        ++ fn pivot equal
        ++ groupAndSort fn cmp greater
    where
        -- Divides a list into three based on a pivot element and a custom ordering function on its elements.
        pivotForQSort :: (a -> b -> Ordering) -> b -> [a] -> ([a], [a], [a])
        pivotForQSort _ _ [] = ([], [], [])
        pivotForQSort cmp pivot (x:xs) =
            let (smaller, equal, greater) = pivotForQSort cmp pivot xs
            in  case cmp x pivot of
                    LT -> (x:smaller, equal, greater)
                    EQ -> (smaller, x:equal, greater)
                    GT -> (smaller, equal, x:greater)
