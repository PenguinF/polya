module Eutherion.Utilities (

       formatAsNumber,
       padLeft,
       conditionalElem,
       listToSimpleArray

       ) where

import Data.Array

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

-- Pads a list on the left with an element up to a maximum total length.
padLeft :: a -> Int -> [a] -> [a]
padLeft c n s = replicate (n - length s) c ++ s

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
