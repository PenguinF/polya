module Eutherion.Polynomial (

       Polynomial,
       VarExpression

       ) where

import Eutherion.Utilities
import Eutherion.CommutativeRing


-- Polynomials on arbitrary commutative rings, with a divisor.
-- This divisor is 2 in example polynomial ½(x + 4)³.
-- 'r' is the ring type, 'v' the type used for variables.

-- Const r r is a constant scalar value. Const 2 3 represents 2/3.
-- Expr (VarExpression r v) r is an expression on variables.
-- ½(x + 4)³ is represented by:
-- VarExpression (Exp (Add 4 [Var 'x']) 3) 2

-- Invariants (0 == r_zero, 1 == r_one):
--
-- 'd' in 'Const x d' is never equal to 0.
-- 'd' in 'Expr e d' is never equal to 0.
-- 'es' in 'Add c es' is always non-empty.
-- 'es' in 'Mult k es' is always non-empty.
-- 'k' in 'Mult k es' is never equal to 0.
-- 'n' in 'Exp e n' is 2 or higher.
-- 'Add 0 [e]' is always simplified to 'e'.
-- 'Mult 1 [e]' is always simplified to 'e'.
-- No element of 'es' in 'Add c es' is itself an Add expression.
-- No element of 'es' in 'Mult k es' is itself a Mult expression.
-- 'e' in 'Exp e n' is never an Exp expression.

data Polynomial r v = Const r r
                    | Expr (VarExpression r v) r
                    deriving Eq

data VarExpression r v = Var v
                       | Add r [VarExpression r v]
                       | Mult r [VarExpression r v]
                       | Exp (VarExpression r v) Integer
                       deriving Eq



-- Show instance for polynomials
-- ShowablePolynomialVariable type class for assigning functions to display variables.
-- A Show instance does not work for characters because they are generated with quotes.

class ShowablePolynomialVariable v where
    showVar :: v -> String

instance ShowablePolynomialVariable Char where
    showVar x = [x]

instance Show a => ShowablePolynomialVariable [a] where
    showVar = show

instance (Show a, Show b) => ShowablePolynomialVariable (a, b) where
    showVar = show

instance (CommutativeRing r, Show r, ShowablePolynomialVariable v) => Show (Polynomial r v) where
    -- Sad that WinGHCi output does not understand Unicode characters.
    -- Using the good old ^ notation therefore.
    -- See showPoly below for the preferred implementation.
    show = showPolynomial (\n -> "^" ++ show n)

-- Shows polynomials.
showPoly :: (CommutativeRing r, Show r, ShowablePolynomialVariable v) => Polynomial r v -> String
showPoly = showPolynomial (\n -> formatAsNumber exponentCharacterLookup n)

-- Shows polynomials given a function to display exponents.
showPolynomial :: (CommutativeRing r, Show r, ShowablePolynomialVariable v) => (Integer -> String) -> Polynomial r v -> String
showPolynomial expShow e =
    case e of
        Const n d          -> withDivisor (show n) d False
        Expr (Add n es) d  -> withDivisor (shv 0 (Add n es)) d True
        Expr e d           -> withDivisor (shv 0 e) d False
    where
        withDivisor s d needBrackets =
            case d of
                d | d == r_one -> s
                d              -> conditionalElem needBrackets '('
                                  ++ s
                                  ++ conditionalElem needBrackets ')'
                                  ++ " / "
                                  ++ show d

        -- With a current operator precedence level which determines when brackets are generated.
        shv p e =
            case e of
                Var x     -> showVar x
                Add n es  -> conditionalElem (p > 0) '('
                             ++ formatSum n es
                             ++ conditionalElem (p > 0) ')'
                Mult n es -> conditionalElem (p > 1) '('
                             ++ formatProduct n es
                             ++ conditionalElem (p > 1) ')'
                Exp e n   -> shv 2 e ++ expShow n

        formatSum k es =
            let formatted = joinList genAddInfix formatAddOp (zip [0..] es)
            in  case k of
                    k | k == r_zero    -> formatted
                    k | r_isNegative k -> formatted ++ " - " ++ show (r_min k)
                    k                  -> formatted ++ " + " ++ show k
            where
                -- Special case: steal minus sign from multiplications.
                genAddInfix _ (_, x) =
                    case x of
                        Mult n es | r_isNegative n -> " - "
                        x                          -> " + "
                formatAddOp (i, x) =
                    case i of
                        0 -> shv 0 x  -- First operand unchanged (e.g. "-x - 2")
                        i -> case x of
                                Mult n es | r_isNegative n -> shv 0 (Mult (r_min n) es)
                                x                          -> shv 0 x

        formatProduct k es =
            let formatted = joinList genMultInfix formatMultOp es
            in  case k of
                    k | k == (r_min r_one) -> "-" ++ formatted
                    k | k == r_one         -> formatted
                    k                      -> show k ++ formatted
            where
                genMultInfix _ _ = ""
                formatMultOp x = shv 1 x
