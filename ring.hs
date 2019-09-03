module Eutherion.CommutativeRing (

       CommutativeRing,
       r_add,
       r_min,
       r_mult,
       r_zero,
       r_one,
       r_isNegative,
       r_abs,
       r_ones,
       r_exp,
       r_gcd_div

       ) where


-- Structures functions according to these laws:
-- (x + y) + z === x + (y + z)
-- x + y       === y + x
-- r_zero + x  === x
-- x + (-x)    === r_zero
-- (x . y) . z === x . (y . z)
-- x . y       === y . x
-- r_one . x   === x
-- x . (y + z) === (x . y) + (x . z)
class Eq a => CommutativeRing a where
    r_add  :: a -> a -> a  -- add operation (+)
    r_min  :: a -> a       -- inverse operation (-)
    r_mult :: a -> a -> a  -- multiply operation (.)
    r_zero :: a            -- identity under add (0)
    r_one  :: a            -- identity under multiplication (1)

    -- Optional function which has an effect on how values smaller than zero are shown.
    -- E.g. x - 2 versus x + -2
    r_isNegative :: a -> Bool
    r_isNegative _ = False

    -- If r_isNegative, returns its inverse, otherwise just the value.
    -- Assumes that if 'r_isNegative x == True', 'r_isNegative $ r_min x == False'.
    r_abs :: a -> a
    r_abs x
        | r_isNegative x = r_min x
        | otherwise      = x

    -- Repeatedly adds r_one to itself, like a multiplication.
    -- Essentially defines a morphism between integers and the set of operands of the ring.
    -- Instances can override with more efficient implementations.
    r_ones :: Integer -> a
    r_ones n
        | n < 0     = r_min $ r_ones (-n)
        | n == 0    = r_zero
        | n == 1    = r_one
        | otherwise = r_one `r_add` (r_ones (n - 1))

    -- Repeatedly multiplies an operand with itself.
    -- Instances can override with more efficient implementations.
    r_exp :: a -> Integer -> a
    r_exp x n
        | n < 0     = error "Negative exponents not allowed"
        | n == 0    = r_one
        | n == 1    = x
        | otherwise = x `r_mult` (r_exp x (n - 1))

    -- r_gcd_div x y returns a triple (gcd, x', y'), with properties:
    -- gcd `r_mult` x' == x
    -- gcd `r_mult` y' == y
    -- gcd is the 'largest' element for which this is true.
    r_gcd_div :: a -> a -> (a, a, a)
    r_gcd_div x y = (r_one, x, y)

instance CommutativeRing Integer where
    r_add        = (+)
    r_min        = \x -> -x
    r_mult       = (*)
    r_zero       = 0
    r_one        = 1
    r_ones       = id
    r_exp        = (^)
    r_isNegative = (< 0)
    r_abs        = abs

instance CommutativeRing Int where
    r_add        = (+)
    r_min        = \x -> -x
    r_mult       = (*)
    r_zero       = 0
    r_one        = 1
    r_ones       = fromInteger
    r_exp        = (^)
    r_isNegative = (< 0)
    r_abs        = abs

instance CommutativeRing Bool where
    r_add False False = False
    r_add True  False = True
    r_add False True  = True
    r_add True  True  = False
    r_min             = id
    r_mult            = (&&)
    r_zero            = False
    r_one             = True
    r_ones n          = n /= 0
    r_abs             = id
