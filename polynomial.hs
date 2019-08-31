module Eutherion.Polynomial (

       Polynomial,
       VarExpression,

       makeConst,
       makeVar,
       addPoly,
       multPoly,
       expPoly,
       divPoly,

       substituteVar,
       substitute,

       ShowablePolynomialVariable,
       showPoly,
       showPolynomial

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
-- 'e' in 'Exp e n' is never an Exp or Mult expression.
-- 'Mult k [Add c es]' is expanded to 'Add (c*k) fs' where k is distributed over each expression in es.

data Polynomial r v = Const r r
                    | Expr (VarExpression r v) r
                    deriving Eq

data VarExpression r v = Var v
                       | Add r [VarExpression r v]
                       | Mult r [VarExpression r v]
                       | Exp (VarExpression r v) Integer
                       deriving Eq


-- Constructor functions

-- Examples:
-- > divPoly ((makeConst :: Integer -> Polynomial Integer Char) 3) 2
-- 3 / 2
-- > multPoly [divPoly ((makeConst :: Integer -> Polynomial Integer Char) 3) 2, divPoly ((makeConst :: Integer -> Polynomial Integer Char) 1) 3]
-- 3 / 6
-- > divPoly (makeVar 'x') 3
-- x / 3
-- > multPoly [divPoly (makeConst 3) 2, divPoly (makeVar 'x') 3]
-- 3x / 6
-- > expPoly (divPoly (makeVar 'x') 3) 2
-- x^2 / 9
-- > multPoly [divPoly (makeConst 3) 2, expPoly (divPoly (makeVar 'x') 3) 2]
-- 3x^2 / 18
-- > addPoly [makeVar 'x', divPoly (makeConst 3) 4, divPoly (makeConst 4) 3]
-- (12x + 25) / 12

makeConst :: CommutativeRing r => r -> Polynomial r v
makeConst c = Const c r_one

makeVar :: CommutativeRing r => v -> Polynomial r v
makeVar x = Expr (Var x) r_one

-- Extracts all constants and embedded Add expressions from a list of polynomials.
-- Assumes all operands have already been normalized.
-- (Private)
extractConstantsAndAddOperands :: CommutativeRing r => [Polynomial r v] -> (r, [VarExpression r v], r)
extractConstantsAndAddOperands es = extractConstantsAndAddOperands' r_one es
    where
        -- Say that our divisors list looks like this:
        -- [2, 3, 5, 1, 1, 3]
        --
        -- Then we want to multiply each element by the product of all other elements
        -- to get a common divisor:
        -- [ 2,  3,  5,  1,  1,  3]
        -- [45, 30, 18, 90, 90, 30]
        -- ------------------------ *
        -- [90, 90, 90, 90, 90, 90]
        --
        -- Without a division operator or a least common multiple (which implies an ordering
        -- on the ring elements), this must be done by using r_mult operations only.
        --
        -- One way to do this is to associate each element with the product of all elements before and after it, like this:
        -- [ 2,  3, 5,  1,  1,  3]
        -- [ 1,  2, 6, 30, 30, 30] (before)
        -- [45, 15, 3,  3,  3,  1] (after)
        --
        -- The 'before' list can be done by adding an extra 'running value' parameter (beforeProduct),
        -- the 'after' list by using the returned divisor value.
        extractConstantsAndAddOperands' :: CommutativeRing r => r -> [Polynomial r v] -> (r, [VarExpression r v], r)
        extractConstantsAndAddOperands' beforeProduct es =
            case es of
                []                      -> (r_zero, [], r_one)
                Const n d : es          -> let (n', vs, d') = extractConstantsAndAddOperands' (beforeProduct `r_mult` d) es
                                               multiplier   = beforeProduct `r_mult` d'
                                               n''          = (n `r_mult` multiplier) `r_add` n'
                                           in  (n'', vs, d' `r_mult` d)
                Expr (Add n es') d : es -> let (n', vs, d') = extractConstantsAndAddOperands' (beforeProduct `r_mult` d) es
                                               multiplier   = beforeProduct `r_mult` d'
                                               n''          = (n `r_mult` multiplier) `r_add` n'
                                           in  (n'', map (distribute multiplier) es' ++ vs, d' `r_mult` d)
                Expr e d : es           -> let (n', vs, d') = extractConstantsAndAddOperands' (beforeProduct `r_mult` d) es
                                               multiplier   = beforeProduct `r_mult` d'
                                           in  (n', distribute multiplier e : vs, d' `r_mult` d)

        distribute multiplier e
            | multiplier == r_one = e
            | otherwise           = case e of
                                        Mult k es -> Mult (multiplier `r_mult` k) es
                                        e         -> Mult multiplier [e]

-- Adds a list of polynomials to form a new polynomial.
addPoly :: CommutativeRing r => [Polynomial r v] -> Polynomial r v
addPoly ps =
    case combineSum $ extractConstantsAndAddOperands ps of
        (sum, [], d)                  -> Const sum d
        (sum, [e], d) | sum == r_zero -> Expr e d
        (sum, es, d)                  -> Expr (Add sum es) d
    where
        -- (m ∙ x) + (n ∙ x) -> (m + n) ∙ x
        combineSum (sum, es, d) = (sum, groupAndSort combineGroupedAddTerms (\x -> \y -> GT) (map makeAddTerm es), d)
        combineGroupedAddTerms t [] =
            case t of
                (k, es)  | k == r_zero -> []
                (k, [e]) | k == r_one  -> [e]
                (k, es)                -> [Mult k es]
        combineGroupedAddTerms (k, es) ((k2, _) : ts) = combineGroupedAddTerms (k `r_add` k2, es) ts

-- Extracts all constants and embedded Mult expressions from a list of polynomials.
-- Assumes all operands have already been normalized.
-- (Private)
extractConstantsAndMultOperands :: CommutativeRing r => [Polynomial r v] -> (r, [VarExpression r v], r)
extractConstantsAndMultOperands es =
    case es of
        []                       -> (r_one, [], r_one)
        Const n d : es           -> let (n', vs, d') = extractConstantsAndMultOperands es
                                    in  (n `r_mult` n', vs, d `r_mult` d')
        Expr (Mult n es') d : es -> let (n', vs, d') = extractConstantsAndMultOperands es
                                    in  (n `r_mult` n', es' ++ vs, d `r_mult` d')
        Expr e d : es            -> let (n', vs, d') = extractConstantsAndMultOperands es
                                    in  (n', e : vs, d `r_mult` d')

-- Distributes a multiplier over a sum.
-- k(x + y) -> kx + ky
-- (Private)
-- Can't simplify this function, because it's not guaranteed to preserve its inner structure.
-- This is because there could exist elements that when multiplied together yield r_zero or r_one,
-- like in the clock group where (6 * 2) mod 12 = 0.
-- Example expression with simplification:
--     [6(2x + y + 3)] mod 12
--   = [12x + 6y + 18] mod 12
--   = [6y + 6] mod 12
distributeMultiplier :: CommutativeRing r => r -> r -> [VarExpression r v] -> Polynomial r v
distributeMultiplier k c es = addPoly (Const (c `r_mult` k) r_one : [multPoly [Const k r_one, Expr e r_one] | e <- es])

-- Multiplies a list of polynomials to form a new polynomial.
multPoly :: CommutativeRing r => [Polynomial r v] -> Polynomial r v
multPoly ps =
    case combineProduct $ extractConstantsAndMultOperands ps of
        (product, [], d)                      -> Const product d  -- Result of e.g. substituting 2 for 'x' in '3x': substituteVar 'x' (mp "2") (mp "3x")
        (product, es, d)  | product == r_zero -> Const r_zero r_one
        (product, [e], d) | product == r_one  -> Expr e d
        (product, [Add c es], d)              -> divPoly (distributeMultiplier product c es) d
        (product, es, d)                      -> Expr (Mult product es) d
    where
        -- x^m * x^n -> x^(m+n)
        combineProduct (product, es, d) = (product, groupAndSort combineGroupedMultTerms (\x -> \y -> GT) (map makeMultTerm es), d)
        combineGroupedMultTerms ts _ =
            case ts of
                (e, n) | n == 1 -> [e]
                (e, n)          -> [Exp e n]

-- Distributes an exponent over a product.
-- (xy)^n -> x^n * y^n
-- (Private)
distributeExponent :: CommutativeRing r => r -> [VarExpression r v] -> Integer -> Polynomial r v
-- Recurse over expPoly because some terms may be Exp expressions.
distributeExponent k es n = multPoly (Const (k `r_exp` n) r_one : [expPoly (Expr e r_one) n | e <- es])

-- Raises a polynomial to a power.
expPoly :: CommutativeRing r => Polynomial r v -> Integer -> Polynomial r v
expPoly p n
    -- Disallow zero exponent because of 0^0 and x^0. Purpose of this
    -- polynomial type is not to solve equations to find those 'x' values
    -- for which the result polynomial value is undefined.
    | n <= 0    = error "Zero or negative exponents not allowed"
    | n == 1    = p
    | otherwise =
        case p of
            Const c d          -> Const (c `r_exp` n) (d `r_exp` n)
            Expr (Exp e n') d  -> Expr (Exp e (n * n')) (d `r_exp` n)
            Expr (Mult k es) d -> divPoly (distributeExponent k es n) (d `r_exp` n)
            Expr e d           -> Expr (Exp e n) (d `r_exp` n)

divPoly :: CommutativeRing r => Polynomial r v -> r -> Polynomial r v
divPoly p d
    | d == r_zero = error "Division by zero"
    | otherwise   =
        case p of
            Const c d' -> Const c (d `r_mult` d')
            Expr e d'  -> Expr e (d `r_mult` d')




-- Substitution functions

-- Substitutes variable x with polynomial p in polynomial q.
substituteVar :: (CommutativeRing r, Ord v) => v -> Polynomial r v -> Polynomial r v -> Polynomial r v
substituteVar x p q = substitute q (replaceIfEqual x p)
    where
        replaceIfEqual :: (CommutativeRing r, Ord v) => v -> Polynomial r v -> v -> Polynomial r v
        replaceIfEqual x p y | x == y    = p
                             | otherwise = makeVar y

substitute :: CommutativeRing r => Polynomial r v -> (v -> Polynomial r w) -> Polynomial r w
substitute e f =
    case e of
        Const n d -> Const n d
        Expr e d  -> divPoly (substitute' f e) d
    where
        substitute' f e =
            case e of
                Var y     -> f y
                Add k es  -> addPoly (makeConst k : (map (substitute' f) es))
                Mult k es -> multPoly (makeConst k : (map (substitute' f) es))
                Exp e n   -> expPoly (substitute' f e) n




-- Private ordering functions

-- Chains orderings by priority.
orderBy :: Ordering -> Ordering -> Ordering
orderBy x y =
    case x of
        EQ -> y
        x  -> x

-- Normalizes everything to a Mult expression for comparison within an Add expression.
makeAddTerm :: CommutativeRing r => VarExpression r v -> (r, [VarExpression r v])
makeAddTerm e =
    case e of
        Mult k es -> (k, es)
        e         -> (r_one, [e])

-- Compares two terms of an Add expression. 'Lesser' terms go in front.
-- Assume by induction on structure that subexpressions are normalized already.
compareAddTerm :: (CommutativeRing r, Ord r, Ord v) => (r, [VarExpression r v]) -> (r, [VarExpression r v]) -> Ordering
compareAddTerm (m, xs) (n, ys) =
    case (m, xs, n, ys) of
        (m, [], n, [])     -> compare m n
        (_, [], _, _)      -> GT
        (_, _, _, [])      -> LT
        (m, x:xs, n, y:ys) -> orderBy (compareMultTerm (makeMultTerm x) (makeMultTerm y)) (compareAddTerm (m, xs) (n, ys))

-- Normalizes everything to an Exp expression for comparison within a Mult expression.
makeMultTerm :: CommutativeRing r => VarExpression r v -> (VarExpression r v, Integer)
makeMultTerm e =
    case e of
        Exp e n -> (e, n)
        e       -> (e, 1)

-- Compares two terms of a Mult expression. 'Lesser' terms go in front.
-- Assume by induction on structure that subexpressions are normalized already.
compareMultTerm :: (CommutativeRing r, Ord r, Ord v) => (VarExpression r v, Integer) -> (VarExpression r v, Integer) -> Ordering
compareMultTerm (x, m) (y, n) = orderBy (compareExpTerm x y) (desc $ compare m n)

-- Compares two terms of an Exp expression. 'Lesser' terms go in front.
-- Assume by induction on structure that subexpressions are normalized already.
compareExpTerm :: (CommutativeRing r, Ord r, Ord v) => VarExpression r v -> VarExpression r v -> Ordering
compareExpTerm e f =
    case (e, f) of
        (Var x, Var y)     -> compare x y
        (Var _, _)         -> LT
        (_, Var _)         -> GT
        (Add m x, Add n y) -> compareAddTerm (m, x) (n, y)




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




instance Functor (Polynomial r) where
    fmap f e =
        case e of
            Const n d -> Const n d
            Expr e d  -> Expr (fmapVarExpression f e) d
        where
            fmapVarExpression f e =
                case e of
                    Var x     -> Var (f x)
                    Add c es  -> Add c (map (fmapVarExpression f) es)
                    Mult k es -> Mult k (map (fmapVarExpression f) es)
                    Exp e n   -> Exp (fmapVarExpression f e) n




-- CommutativeRing instance for Polynomial

-- Maybe require Ord b constraint rather than Eq b, like Set, so normalization is more efficient?
instance (CommutativeRing a, Eq b) => CommutativeRing (Polynomial a b) where
    r_zero       = makeConst r_zero
    r_one        = makeConst r_one
    r_add p q    = addPoly [p, q]
    r_mult p q   = multPoly [p, q]
    r_exp p n    = expPoly p n
    r_min p      = multPoly [makeConst (r_min r_one), p] -- TODO: r_add p (r_min p) must always be r_zero.
    r_isNegative = r_isNegative'
        where
            r_isNegative' e =
                case e of
                    Const k d -> r_isNegative k
                    Expr e d  -> r_isNegative'' e
            r_isNegative'' e =
                case e of
                    Var y     -> False
                    Add k es  -> False
                    Mult k es -> r_isNegative k
                    Exp e n   -> False
