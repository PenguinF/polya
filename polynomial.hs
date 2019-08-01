module Eutherion.Polynomial (

       Polynomial,
       VarExpression

       ) where


-- Polynomials on arbitrary commutative rings, with a scaling factor which acts
-- as a divisor. This scaling factor is 2 in example polynomial ½(x + 4)³.
-- 'r' is the ring type, 'v' the type used for variables.

-- Const r r is a constant scalar value. Const 2 3 represents 2/3.
-- Expr (VarExpression r v) r is an expression on variables, with a scaling factor.
-- ½(x + 4)³ is represented by:
-- PolyExpression (Exp (Add 4 [Var 'x']) 3) 2

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
