module Eutherion.Utilities (

       swap,
       fst3,
       snd3,
       thd3,
       formatAsNumber,
       exponentCharacterLookup,
       expToNumber,
       padLeft,
       padRight,
       conditionalElem,
       listToSimpleArray,
       joinList

       ) where

import Data.Array

swap :: (a -> b -> c) -> b -> a -> c
swap f x y = f y x

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

-- Converts a list to a one-dimensional array with lower bound 0.
listToSimpleArray :: [a] -> Array Int a
listToSimpleArray xs =
    let n      = length xs
        zipped = zip [0..] xs
    in  array (0, n - 1) zipped

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
