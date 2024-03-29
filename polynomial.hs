module Eutherion.Polynomial (

       Polynomial,

       polyZero,
       makeConst,
       makeRational,
       makeVar,
       addPoly,
       multPoly,
       expPoly,
       divPoly,

       substituteVar,
       substitute,

       expand,
       coefficient,

       differentiate,
       differentiateBy,

       ShowablePolynomialVariable,
       showVar,
       showPoly,
       showPolynomial

       ) where

import Data.Foldable

import Eutherion.Utilities
import Eutherion.CommutativeRing
import Eutherion.Combinatorics


-- Polynomials on arbitrary commutative rings, with a divisor.
-- This divisor is 2 in example polynomial ½(x + 4)³.
-- 'r' is the ring type, 'v' the type used for variables.

-- Const r is a constant scalar value. Polynomial (Const 2) 3 represents 2/3.
-- Expr (VarExpression r v) is an expression on variables.
-- ½(x + 4)³ is represented by:
-- Polynomial (Expr (Exp (Add 4 [Var 'x']) 3)) 2

-- Invariants (0 == r_zero, 1 == r_one):
--
-- 'd' in 'Polynomial p d' is never equal to 0.
-- If 'x' in 'Polynomial (Const x) d' is equal to 0, then 'd' is equal to 1.
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

data Polynomial r v = Polynomial (Expression r v) r
                    deriving Eq

data Expression r v = Const r
                    | Expr (VarExpression r v)
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
-- 1 / 2
-- > divPoly (makeVar 'x') 3
-- x / 3
-- > multPoly [divPoly (makeConst 3) 2, divPoly (makeVar 'x') 3]
-- x / 2
-- > expPoly (divPoly (makeVar 'x') 3) 2
-- x^2 / 9
-- > multPoly [divPoly (makeConst 3) 2, expPoly (divPoly (makeVar 'x') 3) 2]
-- x^2 / 6
-- > addPoly [makeVar 'x', divPoly (makeConst 3) 4, divPoly (makeConst 4) 3]
-- (12x + 25) / 12

polyZero :: CommutativeRing r => Polynomial r v
polyZero = Polynomial (Const r_zero) r_one

-- (Private)
makePolyZero :: CommutativeRing r => r -> Polynomial r v
makePolyZero d
    | d == r_zero = error "Division by zero"
    | otherwise   = polyZero

-- (Private)
makePoly :: CommutativeRing r => Expression r v -> r -> Polynomial r v
makePoly p d
    | d == r_zero = error "Division by zero"
    | otherwise   = Polynomial p d

makeConst :: CommutativeRing r => r -> Polynomial r v
makeConst c = Polynomial (Const c) r_one

makeRational :: CommutativeRing r => r -> r -> Polynomial r v
makeRational c d
    | c == r_zero = makePolyZero d
    | otherwise   = let (c', d') = r_div_by_gcd c d
                    in  makePoly (Const c') d'

-- (Private)
liftVarExpression :: CommutativeRing r => VarExpression r v -> Polynomial r v
liftVarExpression e = Polynomial (Expr e) r_one

makeVar :: CommutativeRing r => v -> Polynomial r v
makeVar x = liftVarExpression (Var x)

-- (Private)
makeAddPolyGcd :: CommutativeRing r => r -> [VarExpression r v] -> r -> Polynomial r v
makeAddPolyGcd k es d = makeAddPolyGcd' (foldl' gcdWithVarExpression (d `r_gcd` k) es) k es d
    where
        gcdWithVarExpression gcd e =
            case e of
                Mult k _ -> gcd `r_gcd` k
                _        -> r_one

        makeAddPolyGcd' gcd k es d =
            let (k', _) = r_div_by_gcd k gcd
                es'     = map divVarExpression es
                (d', _) = r_div_by_gcd d gcd
            in  makePoly (Expr (Add k' es')) d'
            where
                divVarExpression (Mult k es) = let (k', _) = r_div_by_gcd k gcd in Mult k' es
                divVarExpression e           = e

-- (Private)
makeMult :: CommutativeRing r => r -> [VarExpression r v] -> [VarExpression r v]
makeMult k es =
    case es of
        es  | k == r_zero -> []
        [e] | k == r_one  -> [e]
        es                -> [Mult k es]

-- (Private)
makeMultPoly :: CommutativeRing r => r -> [VarExpression r v] -> r -> Polynomial r v
makeMultPoly k es d =
    let (k', d') = r_div_by_gcd k d
        es'      = makeMult k' es
    in case es' of
        []  -> makePolyZero d'
        [e] -> makePoly (Expr e) d'


-- Extracts all constants and embedded Add expressions from a list of polynomials.
-- Assumes all operands have already been normalized.
-- (Private)
extractConstantsAndAddOperands :: (CommutativeRing r, Foldable f) => f (Polynomial r v) -> (r, [VarExpression r v], r)
extractConstantsAndAddOperands = foldl' addConstantsAndOperands (r_zero, [], r_one) . snd . foldl' zipWithBeforeProduct (r_one, [])
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
        zipWithBeforeProduct (beforeProduct, ps) p@(Polynomial _ d) = (beforeProduct `r_mult` d, (beforeProduct, p) : ps)

        addConstantsAndOperands (n, vs, afterProduct) (beforeProduct, Polynomial p d) =
            let multiplier = beforeProduct `r_mult` afterProduct
            in case p of
                Const m         -> ((m `r_mult` multiplier) `r_add` n, vs,                                            d `r_mult` afterProduct)
                Expr (Add m es) -> ((m `r_mult` multiplier) `r_add` n, concat (map (distribute multiplier) es) ++ vs, d `r_mult` afterProduct)
                Expr e          -> (n,                                 distribute multiplier e ++ vs,                 d `r_mult` afterProduct)
            where
                distribute multiplier e =
                    case e of
                        Mult k es -> makeMult (multiplier `r_mult` k) es
                        e         -> makeMult multiplier [e]

-- Adds a list of polynomials to form a new polynomial.
addPoly :: (CommutativeRing r, Ord r, Ord v, Foldable f) => f (Polynomial r v) -> Polynomial r v
addPoly ps =
    case combineSum $ extractConstantsAndAddOperands ps of
        (sum, [], d)                  -> makeRational sum d
        (sum, [e], d) | sum == r_zero -> makePoly (Expr e) d
        (sum, es, d)                  -> makeAddPolyGcd sum es d
    where
        -- (m ∙ x) + (n ∙ x) -> (m + n) ∙ x
        combineSum (sum, es, d) = (sum, groupAndSort combineGroupedAddTerms (compareAddTerm True) (map makeAddTerm es), d)
        combineGroupedAddTerms (k, es) []             = makeMult k es
        combineGroupedAddTerms (k, es) ((k2, _) : ts) = combineGroupedAddTerms (k `r_add` k2, es) ts

-- Extracts all constants and embedded Mult expressions from a list of polynomials.
-- Assumes all operands have already been normalized.
-- (Private)
extractConstantsAndMultOperands :: (CommutativeRing r, Foldable f) => f (Polynomial r v) -> (r, [VarExpression r v], r)
extractConstantsAndMultOperands = foldl' multConstantsAndOperands (r_one, [], r_one)
    where
        multConstantsAndOperands (n, vs, d') (Polynomial p d) =
            case p of
                Const m          -> (m `r_mult` n, vs,       d `r_mult` d')
                Expr (Mult m es) -> (m `r_mult` n, es ++ vs, d `r_mult` d')
                Expr e           -> (n,            e : vs,   d `r_mult` d')

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
distributeMultiplier :: (CommutativeRing r, Ord r, Ord v) => r -> r -> [VarExpression r v] -> Polynomial r v
distributeMultiplier k c es = addPoly $ makeConst (c `r_mult` k) : [multPoly [makeConst k, liftVarExpression e] | e <- es]

-- Multiplies a list of polynomials to form a new polynomial.
multPoly :: (CommutativeRing r, Ord r, Ord v, Foldable f) => f (Polynomial r v) -> Polynomial r v
multPoly ps =
    case combineProduct $ extractConstantsAndMultOperands ps of
        (product, [], d)                      -> makeRational product d  -- Result of e.g. substituting 2 for 'x' in '3x': substituteVar 'x' (mp "2") (mp "3x")
        (product, es, d)  | product == r_zero -> makePolyZero d
        (product, [e], d) | product == r_one  -> makePoly (Expr e) d
        (product, [Add c es], d)              -> divPoly (distributeMultiplier product c es) d
        (product, es, d)                      -> makeMultPoly product es d
    where
        -- x^m * x^n -> x^(m+n)
        combineProduct (product, es, d) = (product, groupAndSort combineGroupedMultTerms (compareMultTerm True) (map makeMultTerm es), d)
        combineGroupedMultTerms t [] =
            case t of
                (e, n) | n == 1 -> [e]
                (e, n)          -> [Exp e n]
        combineGroupedMultTerms (e, n) ((_, n2) : ts) = combineGroupedMultTerms (e, n + n2) ts

-- Distributes an exponent over a product.
-- (xy)^n -> x^n * y^n
-- (Private)
distributeExponent :: (CommutativeRing r, Ord r, Ord v) => r -> [VarExpression r v] -> Integer -> Polynomial r v
-- Recurse over expPoly because some terms may be Exp expressions.
distributeExponent k es n = multPoly $ makeConst (k `r_exp` n) : map ((`expPoly` n) . liftVarExpression) es

-- Raises a polynomial to a power.
expPoly :: (CommutativeRing r, Ord r, Ord v) => Polynomial r v -> Integer -> Polynomial r v
expPoly (Polynomial p d) n
    -- Disallow zero exponent because of 0^0 and x^0. Purpose of this
    -- polynomial type is not to solve equations to find those 'x' values
    -- for which the result polynomial value is undefined.
    | n <= 0    = error "Zero or negative exponents not allowed"
    | n == 1    = Polynomial p d
    | otherwise =
        let d' = d `r_exp` n
        in case p of
            Const c          -> makeRational (c `r_exp` n) d'
            Expr (Exp e m)   -> makePoly (Expr (Exp e (m * n))) d'
            Expr (Mult k es) -> divPoly (distributeExponent k es n) d'
            Expr e           -> makePoly (Expr (Exp e n)) d'

divPoly :: CommutativeRing r => Polynomial r v -> r -> Polynomial r v
divPoly (Polynomial p d1) d2 =
    let d' = d1 `r_mult` d2
    in case p of
        Const c          -> makeRational c d'
        Expr (Mult k es) -> makeMultPoly k es d'
        Expr (Add k es)  -> makeAddPolyGcd k es d'
        Expr e           -> makePoly (Expr e) d'




-- Substitution functions

-- Substitutes variable x with polynomial p in polynomial q.
substituteVar :: (CommutativeRing r, Ord r, Ord v) => v -> Polynomial r v -> Polynomial r v -> Polynomial r v
substituteVar x p q = substitute q (replaceIfEqual x p)
    where
        replaceIfEqual :: (CommutativeRing r, Ord v) => v -> Polynomial r v -> v -> Polynomial r v
        replaceIfEqual x p y | x == y    = p
                             | otherwise = makeVar y

substitute :: (CommutativeRing r, Ord r, Ord v, Ord w) => Polynomial r v -> (v -> Polynomial r w) -> Polynomial r w
substitute (Polynomial p d) f =
    case p of
        Const n -> Polynomial (Const n) d
        Expr e  -> divPoly (substitute' f e) d
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
compareAddTerm :: (CommutativeRing r, Ord r, Ord v) => Bool -> (r, [VarExpression r v]) -> (r, [VarExpression r v]) -> Ordering
compareAddTerm ignoreCoefficient (m, xs) (n, ys) =
    case ignoreCoefficient of
        False -> orderBy (compareProduct xs ys) (compare n m)  -- 3x + 1 > 2x + 1
        True  -> compareProduct xs ys

compareProduct :: (CommutativeRing r, Ord r, Ord v) => [VarExpression r v] -> [VarExpression r v] -> Ordering
compareProduct xs ys =
    case (xs, ys) of
        ([], [])     -> EQ
        ([], _)      -> GT
        (_, [])      -> LT
        (x:xs, y:ys) -> orderBy (compareMultTerm False (makeMultTerm x) (makeMultTerm y)) (compareProduct xs ys)

-- Normalizes everything to an Exp expression for comparison within a Mult expression.
makeMultTerm :: CommutativeRing r => VarExpression r v -> (VarExpression r v, Integer)
makeMultTerm e =
    case e of
        Exp e n -> (e, n)
        e       -> (e, 1)

-- Compares two terms of a Mult expression. 'Lesser' terms go in front.
-- Assume by induction on structure that subexpressions are normalized already.
compareMultTerm :: (CommutativeRing r, Ord r, Ord v) => Bool -> (VarExpression r v, Integer) -> (VarExpression r v, Integer) -> Ordering
compareMultTerm ignoreCoefficient (x, m) (y, n) =
    case ignoreCoefficient of
        False -> orderBy (compareExpTerm x y) (compare n m)
        True  -> compareExpTerm x y

-- Compares two terms of an Exp expression. 'Lesser' terms go in front.
-- Assume by induction on structure that subexpressions are normalized already.
compareExpTerm :: (CommutativeRing r, Ord r, Ord v) => VarExpression r v -> VarExpression r v -> Ordering
compareExpTerm e f =
    case (e, f) of
        (Var x, Var y)     -> compare x y
        (Var _, _)         -> LT
        (_, Var _)         -> GT
        (Add m x, Add n y) -> orderBy (compareSum x y) (compare n m)

compareSum :: (CommutativeRing r, Ord r, Ord v) => [VarExpression r v] -> [VarExpression r v] -> Ordering
compareSum xs ys =
    case (xs, ys) of
        ([], [])     -> EQ
        ([], _)      -> GT
        (_, [])      -> LT
        (x:xs, y:ys) -> orderBy (compareAddTerm False (makeAddTerm x) (makeAddTerm y)) (compareSum xs ys)




-- Expand function

-- This helper type normalizes polynomials for the expand function.
-- constant c + any (constant k * any (variable x to the power n))
-- c + ∑ k[i] ∏ x[ij]^n[ij])
-- where all k[i] /= 0, all n[ij] > 0.
type NormVar v       = (v, Integer)
type NormProduct r v = (r, [NormVar v])
type NormSum r v     = (r, [NormProduct r v])

-- Multiplies a non-empty list of products.
multiplyProducts :: (CommutativeRing r, Ord v) => r -> [NormProduct r v] -> NormProduct r v
multiplyProducts c ps =
    case ps of
        [] -> (c, [])  -- Scalar product.
        ps -> let (k, bigProduct) = foldl' multiplyProduct (head ps) (tail ps)
                  sortedProduct   = groupAndSort combineGroupedVars groupByNormVar bigProduct
              in  (c `r_mult` k, sortedProduct)
    where
        combineGroupedVars :: NormVar v -> [NormVar v] -> [NormVar v]
        combineGroupedVars (x, m) xs =
            case xs of
                []            -> [(x, m)]
                (y, n) : vars -> combineGroupedVars (x, m + n) vars

        multiplyProduct :: CommutativeRing r => NormProduct r v -> NormProduct r v -> NormProduct r v
        multiplyProduct (k1, vars1) (k2, vars2) = (k1 `r_mult` k2, vars1 ++ vars2)

        -- Group by variable name (in ascending order).
        groupByNormVar :: Ord v => NormVar v -> NormVar v -> Ordering
        groupByNormVar (x, _) (y, _) = compare x y

gatherScalars :: CommutativeRing r => [NormProduct r v] -> (r, [NormProduct r v])
gatherScalars products =
    case products of
        []               -> (r_zero, [])
        (k, []):products -> let (n, ps) = gatherScalars products
                            in  (k `r_add` n, ps)
        p:products       -> let (n, ps) = gatherScalars products
                            in  (n, p:ps)

expandPower :: (CommutativeRing r, Ord v) => NormSum r v -> Integer -> NormSum r v
expandPower x 1 = x
expandPower (c, products) n =
    case (c, products) of
        (c, [])                      -> (c `r_exp` n, [])
        (c, [product]) | c == r_zero -> (r_zero, [distributeExponent' (n, product)])
        (c, products)  | c == r_zero -> expandPowerOfSumZero products n
        (c, products)                -> expandPowerOfSum (c, products) n
    where
        nonZeroExponent :: (Eq a, Num a) => (a, b) -> Bool
        nonZeroExponent (k, _) =
            case k of
                0 -> False
                _ -> True

        -- Distributes exponent n over all terms in a product.
        distributeExponent' :: CommutativeRing r => (Integer, NormProduct r v) -> NormProduct r v
        distributeExponent' (1, (k, vars)) = (k, vars)
        distributeExponent' (n, (k, vars)) = (k `r_exp` n, [(x, m `r_mult` n) | (x, m) <- vars])

        -- Raises a sum with a zero constant value to some power and expands it.
        expandPowerOfSumZero :: (CommutativeRing r, Ord v) => [NormProduct r v] -> Integer -> NormSum r v
        expandPowerOfSumZero products n = gatherScalars $ map getExpansionTerm $ choose products n
            where
                getExpansionTerm :: (CommutativeRing r, Ord v) => (Integer, [(Integer, NormProduct r v)]) -> NormProduct r v
                getExpansionTerm (coefficient, terms) =
                    let multiplier = r_ones coefficient
                    in  multiplyProducts multiplier $ map distributeExponent' $ filter nonZeroExponent terms

        expandPowerOfSum :: (CommutativeRing r, Ord v) => NormSum r v -> Integer -> NormSum r v
        expandPowerOfSum (c, products) n = gatherScalars $ map getExpansionTerm $ choose (map right products ++ [Left c]) n
            where
                right x = Right x
                unright (k, Right x) = (k, x)

                getExpansionTerm :: (CommutativeRing r, Ord v) => (Integer, [(Integer, Either r (NormProduct r v))]) -> NormProduct r v
                getExpansionTerm (coefficient, varTerms) =
                    let (initTerms, (k, Left c)) = initLast varTerms
                        multiplier               = (r_ones coefficient) `r_mult` (c `r_exp` k)
                    in  multiplyProducts multiplier $ map distributeExponent' $ filter nonZeroExponent $ map unright initTerms

                -- Split a non-empty list into an initial part and a last element.
                initLast :: [a] -> ([a], a)
                initLast [x]    = ([], x)
                initLast (x:xs) = let (ys, y) = initLast xs in (x:ys, y)

expandProduct :: (CommutativeRing r, Ord r, Ord v) => [NormSum r v] -> NormSum r v
expandProduct sums = foldl' multiplySum (head sums) (tail sums)
    where
        multiplySum :: (CommutativeRing r, Ord r, Ord v) => NormSum r v -> NormSum r v -> NormSum r v
        multiplySum (c, x) (d, y) =
            -- Use addSums and multiplyProducts to sort and combine terms.
            let cy = if c == r_zero then [] else [(c `r_mult` k, vs) | (k, vs) <- y]
                dx = if d == r_zero then [] else [(d `r_mult` k, vs) | (k, vs) <- x]
            in  addSums ((c `r_mult` d, cy ++ dx) : [gatherScalars [multiplyProducts r_one [p, q] | p <- x, q <- y]])

addSums :: (CommutativeRing r, Ord r, Ord v) => [NormSum r v] -> NormSum r v
addSums sums =
    let (c, bigSum) = foldl' addSum (head sums) (tail sums)
        sortedSum   = groupAndSort combineGroupedProducts groupByNormProduct bigSum
    in  (c, sortedSum)
    where
        combineGroupedProducts :: CommutativeRing r => NormProduct r v -> [NormProduct r v] -> [NormProduct r v]
        combineGroupedProducts (k1, vars1) xs =
            case xs of
                [] | k1 == r_zero      -> []
                   | otherwise         -> [(k1, vars1)]
                (k2, vars2) : products -> combineGroupedProducts (k1 `r_add` k2, vars1) products

        addSum :: (CommutativeRing r, Ord v) => NormSum r v -> NormSum r v -> NormSum r v
        addSum (c, x) (d, y) = (c `r_add` d, x ++ y)

        -- Group by variable name (in ascending order), then exponent (in descending order).
        -- Assumes NormProducts are already sorted by using groupByNormVar.
        groupByNormProduct :: (CommutativeRing r, Ord v) => NormProduct r v -> NormProduct r v -> Ordering
        groupByNormProduct (_, vars1) (_, vars2) = groupByVars vars1 vars2
            where
                groupByVars :: Ord v => [NormVar v] -> [NormVar v] -> Ordering
                groupByVars vars1 vars2 =
                    case (vars1, vars2) of
                        ([], [])                     -> EQ
                        ([], _)                      -> GT
                        (_, [])                      -> LT
                        ((x, m):vars1, (y, n):vars2) -> orderBy (compare x y) $ orderBy (compare n m) (groupByVars vars1 vars2)

expandVarExpression :: (CommutativeRing r, Ord r, Ord v) => VarExpression r v -> NormSum r v
expandVarExpression e =
    case e of
        Var x     -> (r_zero, [(r_one, [(x, 1)])])
        Exp e n   -> expandPower (expandVarExpression e) n
        Mult c es -> let (k, ps) = expandProduct (map expandVarExpression es)
                     in  (c `r_mult` k, [(c `r_mult` p, v) | (p, v) <- ps])
        Add c es  -> let (k, ps) = addSums (map expandVarExpression es)
                     in  (c `r_add` k, ps)

convertNormSumToPolynomial :: CommutativeRing r => NormSum r v -> r -> Polynomial r v
convertNormSumToPolynomial (c, products) d =
    case (c, products) of
        (c, [])                                       -> makeRational c d
        (c, [(k, [var])]) | c == r_zero && k == r_one -> Polynomial (Expr (convertNormVarToExpression var)) d
        (c, [(k, vars)])  | c == r_zero               -> makeMultPoly k (map convertNormVarToExpression vars) d
        (c, products)                                 -> makeAddPolyGcd c (concat (map convertNormProductToExpression products)) d
    where
        convertNormProductToExpression :: CommutativeRing r => NormProduct r v -> [VarExpression r v]
        convertNormProductToExpression (k, vars) = makeMult k (map convertNormVarToExpression vars)

        convertNormVarToExpression :: NormVar v -> VarExpression r v
        convertNormVarToExpression x =
            case x of
                (x, 1) -> Var x
                (x, n) -> Exp (Var x) n

expand :: (CommutativeRing r, Ord r, Ord v) => Polynomial r v -> Polynomial r v
expand (Polynomial p d) =
    case p of
        Const n -> Polynomial (Const n) d
        Expr e  -> convertNormSumToPolynomial (expandVarExpression e) d

coefficient :: (CommutativeRing r, Ord r, Ord v) => [(v, Integer)] -> Polynomial r v -> Polynomial r v
coefficient vs = coefficient' (groupAndSort combineGroupedVar groupByVar vs)
    where
        groupByVar (x, _) (y, _) = compare x y

        -- Leave out exponents equal to 0.
        combineGroupedVar (x, m) []
            | m == 0    = []
            | otherwise = [(x, m)]
        combineGroupedVar (x, m) ((_, n) : vs) = combineGroupedVar (x, m + n) vs

        matchVar vs (_, ws) = vs == ws

        coefficient' vs (Polynomial p d) =
            case (vs, p) of
                ([], Const c) -> Polynomial (Const c) d
                (_,  Const _) -> polyZero
                ([], Expr e)  -> let (c, _) = expandVarExpression e
                                 in  convertNormSumToPolynomial (c, []) d
                (vs, Expr e)  -> let (_, products) = expandVarExpression e
                                 in  convertNormSumToPolynomial (r_zero, filter (matchVar vs) products) d




-- Differentation

-- Differentiates a polynomial with respect to a variable.
-- > addPoly (makeConst 1 : take 10 (map (makeVar 'x' `expPoly`) [1..]))
-- x^10 + x^9 + x^8 + x^7 + x^6 + x^5 + x^4 + x^3 + x^2 + x + 1
-- > take 12 $ iterate (`differentiate` 'x') $ addPoly (makeConst 1 : take 10 (map (makeVar 'x' `expPoly`) [1..]))
-- [x^10 + x^9 + x^8 + x^7 + x^6 + x^5 + x^4 + x^3 + x^2 + x + 1,
--  10x^9 + 9x^8 + 8x^7 + 7x^6 + 6x^5 + 5x^4 + 4x^3 + 3x^2 + 2x + 1,
--  90x^8 + 72x^7 + 56x^6 + 42x^5 + 30x^4 + 20x^3 + 12x^2 + 6x + 2,
--  720x^7 + 504x^6 + 336x^5 + 210x^4 + 120x^3 + 60x^2 + 24x + 6,
--  5040x^6 + 3024x^5 + 1680x^4 + 840x^3 + 360x^2 + 120x + 24,
--  30240x^5 + 15120x^4 + 6720x^3 + 2520x^2 + 720x + 120,
--  151200x^4 + 60480x^3 + 20160x^2 + 5040x + 720,
--  604800x^3 + 181440x^2 + 40320x + 5040,
--  1814400x^2 + 362880x + 40320,
--  3628800x + 362880,
--  3628800,
--  0]
differentiate  :: (CommutativeRing r, Ord r, Ord v) => Polynomial r v -> v -> Polynomial r v
differentiate p v = differentiateBy p (==v)

differentiateBy :: (CommutativeRing r, Ord r, Ord v) => Polynomial r v -> (v -> Bool) -> Polynomial r v
differentiateBy (Polynomial p d) vPredicate =
    case p of
        Const c -> polyZero
        Expr e  -> divPoly (differentiateBy' e) d
    where
        differentiateBy' e =
            case e of
                Var x     -> if vPredicate x then makeConst r_one else polyZero

                -- Apply chain rule for x^n: (x^n)' -> n(x^n-1) * x'
                Exp t n   -> multPoly [makeConst (r_ones n), expPoly (liftVarExpression t) (n-1), differentiateBy' t]

                -- Apply product rule: (xyz)' -> x'yz + xy'z + xyz'
                Mult c ts -> multPoly [makeConst c, addPoly (map multPoly $ mapOneElementEach (`differentiateBy` vPredicate) $ map liftVarExpression ts)]

                -- Ignore constant, differentiate all terms: (x + y)' -> x' + y'
                Add _ ts  -> addPoly $ fmap differentiateBy' ts




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
showPolynomial expShow (Polynomial p d) =
    case p of
        Const n         -> withDivisor (show n) d False
        Expr (Add n es) -> withDivisor (shv 0 (Add n es)) d True
        Expr e          -> withDivisor (shv 0 e) d False
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
    fmap f p =
        case p of
            Polynomial (Const n) d -> Polynomial (Const n) d
            Polynomial (Expr e) d  -> Polynomial (Expr (fmapVarExpression f e)) d
        where
            fmapVarExpression f e =
                case e of
                    Var x     -> Var (f x)
                    Add c es  -> Add c (map (fmapVarExpression f) es)
                    Mult k es -> Mult k (map (fmapVarExpression f) es)
                    Exp e n   -> Exp (fmapVarExpression f e) n




-- CommutativeRing instance for Polynomial

instance (CommutativeRing a, Ord a, Ord b) => CommutativeRing (Polynomial a b) where
    r_zero       = polyZero
    r_one        = makeConst r_one
    r_ones       = makeConst . r_ones
    r_add p q    = addPoly [p, q]
    r_mult p q   = multPoly [p, q]
    r_exp p n    = expPoly p n
    r_min p      = multPoly [makeConst (r_min r_one), p]
    r_isNegative = r_isNegative'
        where
            r_isNegative' (Polynomial p _) =
                case p of
                    Const k -> r_isNegative k
                    Expr e  -> r_isNegative'' e
            r_isNegative'' e =
                case e of
                    Var y     -> False
                    Add k es  -> False
                    Mult k es -> r_isNegative k
                    Exp e n   -> False
