
import Control.Exception
import Data.List

import Eutherion.Utilities
import Eutherion.Combinatorics
import Eutherion.CommutativeRing
import Eutherion.Polynomial
import Eutherion.Polya




-- Example I: graphs on 1 or more points.

-- Usage:
-- -- Get the number of generated permutation functions which map an edge to another, for graphs with 4 vertices:
-- > length $ enumPermuteGraphEdgeMappings $ undirectedGraph 0 3
-- 24
-- -- Now find out where the edge between vertices 0 and 1 ends up when the third (index 2) permutation function is applied to it:
-- > ((enumPermuteGraphEdgeMappings (undirectedGraph 0 3)) !! 2) (GraphEdge (GraphVertex 0) (GraphVertex 1))
-- GraphEdge (GraphVertex 1) (GraphVertex 2)
-- -- Show the Cayley table of the Pólya group for graphs with 4 vertices:
-- > cayleyTable $ graphPolyaGroup 4

data UndirectedGraph a = UndirectedGraph a a deriving (Show, Eq)

undirectedGraph :: Ord a => a -> a -> UndirectedGraph a
undirectedGraph min max = assert (min <= max) $ UndirectedGraph min max

data GraphVertex a = GraphVertex a deriving (Show, Eq, Ord)

vertex :: Ord a => UndirectedGraph a -> a -> GraphVertex a
vertex (UndirectedGraph vmin vmax) n = assert (vmin <= n && n <= vmax) $ GraphVertex n

data GraphEdge a = GraphEdge (GraphVertex a) (GraphVertex a) deriving (Show, Eq)

enumGraphVertices :: Enum a => UndirectedGraph a -> [GraphVertex a]
enumGraphVertices (UndirectedGraph vmin vmax) = [GraphVertex x | x <- [vmin..vmax]]

enumUndirectedNonReflexiveGraphEdges :: (Enum a, Eq a) => UndirectedGraph a -> [GraphEdge a]
enumUndirectedNonReflexiveGraphEdges (UndirectedGraph vmin vmax) =
    [GraphEdge (GraphVertex v) (GraphVertex w) | v <- [vmin..vmax], w <- [v..vmax], v /= w]

enumPermuteGraphEdgeMappings :: (Enum a, Ord a) => UndirectedGraph a -> [GraphEdge a -> GraphEdge a]
enumPermuteGraphEdgeMappings (UndirectedGraph vmin vmax) =
    [permuteEdge $ permuteVertex vertexPermutation | vertexPermutation <- permutations $ enumGraphVertices (UndirectedGraph vmin vmax)]
    where
        permuteVertex :: Enum b => [GraphVertex a] -> GraphVertex b -> GraphVertex a
        permuteVertex vs (GraphVertex n) = vs !! (fromEnum n - fromEnum vmin)

        permuteEdge :: Ord a => (GraphVertex b -> GraphVertex a) -> GraphEdge b -> GraphEdge a
        permuteEdge permuteVertex (GraphEdge v w) =
            case (permuteVertex v, permuteVertex w) of
                (v', w') | v' <= w'  -> GraphEdge v' w'
                         | otherwise -> GraphEdge w' v'

graphPolyaGroup :: (Ord a, Enum a, Num a) => a -> PolyaGroup (GraphEdge a)
graphPolyaGroup n =
    makePolyaGroup (enumUndirectedNonReflexiveGraphEdges ug) (enumPermuteGraphEdgeMappings ug)
    where
        ug = undirectedGraph 0 (n - 1)




-- Example II: square boards of size 1 and greater.

-- -- Show the Cayley table of the Pólya group for Tic-Tac-Toe boards:
-- > cayleyTable $ sqBoardPolyaGroup 3

data SquareBoard a = SquareBoard a

squareBoard :: (Ord a, Num a) => a -> SquareBoard a
squareBoard size = assert (size > 0) $ SquareBoard size

data SquareBoardCoordinate a = SquareBoardCoordinate a a deriving (Show, Eq)

coordinate :: (Ord a, Enum a) => SquareBoard a -> a -> a -> SquareBoardCoordinate a
coordinate (SquareBoard size) x y = assert (toEnum 0 <= x && x < size && toEnum 0 <= y && y < size) $ SquareBoardCoordinate x y

-- All 8 members of the square board symmetry group - given a board size.
symmetryId    :: p -> a -> a
vFlip         :: Num a => a -> SquareBoardCoordinate a -> SquareBoardCoordinate a
hFlip         :: Num a => a -> SquareBoardCoordinate a -> SquareBoardCoordinate a
slashFlip     :: Num a => a -> SquareBoardCoordinate a -> SquareBoardCoordinate a
backSlashFlip :: p -> SquareBoardCoordinate a -> SquareBoardCoordinate a
rotate90      :: Num a => a -> SquareBoardCoordinate a -> SquareBoardCoordinate a
rotate180     :: Num a => a -> SquareBoardCoordinate a -> SquareBoardCoordinate a
rotate270     :: Num a => a -> SquareBoardCoordinate a -> SquareBoardCoordinate a

-- (0, 0) is assumed to be at the top-left corner of the board.
symmetryId    _                             = id
vFlip         n (SquareBoardCoordinate x y) = SquareBoardCoordinate (n - 1 - x) y
hFlip         n (SquareBoardCoordinate x y) = SquareBoardCoordinate x           (n - 1 - y)
slashFlip     n (SquareBoardCoordinate x y) = SquareBoardCoordinate (n - 1 - y) (n - 1 - x)
backSlashFlip _ (SquareBoardCoordinate x y) = SquareBoardCoordinate y           x
rotate90      n (SquareBoardCoordinate x y) = SquareBoardCoordinate y           (n - 1 - x)
rotate180     n (SquareBoardCoordinate x y) = SquareBoardCoordinate (n - 1 - x) (n - 1 - y)
rotate270     n (SquareBoardCoordinate x y) = SquareBoardCoordinate (n - 1 - y) x

enumBoardSymmetryOperations :: (Ord a, Num a) => SquareBoard a -> [SquareBoardCoordinate a -> SquareBoardCoordinate a]
enumBoardSymmetryOperations (SquareBoard n)
    | n > 1     = [symmetryId n, vFlip n, hFlip n, slashFlip n, backSlashFlip n, rotate90 n, rotate180 n, rotate270 n]
    | otherwise = [symmetryId n]  -- trivial group

sqBoardPolyaGroup :: (Ord a, Enum a, Num a) => a -> PolyaGroup (SquareBoardCoordinate a)
sqBoardPolyaGroup n =
    makePolyaGroup [SquareBoardCoordinate x y | x <- [0..n - 1], y <- [0..n - 1]] (enumBoardSymmetryOperations (squareBoard n))





-- Tokenization for parsing Polynomial Integer Char expressions.
-- Division not supported, just use divPoly for that.
data ExprToken = TkLBracket
               | TkRBracket
               | TkPlus
               | TkMinus
               | TkMultiply
               | TkExpOp
               | TkNumber Integer
               | TkExponent Integer
               | TkVar Char
               deriving (Show)

-- Tokenizes an input string for building expressions.
lexExpr :: String -> [ExprToken]
lexExpr x =
    case x of
        []    -> []
        ' ':s -> lexExpr s
        '(':s -> TkLBracket : lexExpr s
        ')':s -> TkRBracket : lexExpr s
        '+':s -> TkPlus     : lexExpr s
        '-':s -> TkMinus    : lexExpr s
        '*':s -> TkMultiply : lexExpr s
        '.':s -> TkMultiply : lexExpr s
        '∙':s -> TkMultiply : lexExpr s
        '^':s -> TkExpOp    : lexExpr s
        c:s | c >= '0' && c <= '9' -> buildNumber (ord c) s
            | fst (expToNumber c)  -> buildExponent (snd $ expToNumber c) s
            | otherwise            -> TkVar c : lexExpr s
    where
        -- Converts a digit character to an integer value.
        ord c = toInteger ((fromEnum c) - (fromEnum '0'))

        buildNumber n [] = [TkNumber n]
        buildNumber n (c:s)
            | c >= '0' && c <= '9' = buildNumber (10 * n + (ord c)) s
            | otherwise            = TkNumber n : lexExpr (c:s)

        buildExponent n [] = [TkExponent n]
        buildExponent n (c:s)
            | fst (expToNumber c) = buildExponent (10 * n + (snd $ expToNumber c)) s
            | otherwise           = TkExponent n : lexExpr (c:s)

-- Parses a polynomial with constant divisor 1.
parseExpr :: [ExprToken] -> Polynomial Integer Char
parseExpr tokens =
    let (e, remainder) = parseSum tokens
    in  case remainder of
            [] -> e
            _  -> parseError "Expected EOF"
    where
        parseError msg = error ("Parse error: " ++ msg)

        parseExprThenRBracket tokens =
            let (e, remainder) = parseSum tokens
            in  case remainder of
                    TkRBracket:ts -> (e, ts)
                    _             -> parseError "Expected ')'"

        parseOperand tokens =
            case tokens of
                TkLBracket:ts -> parseExprThenRBracket ts
                TkNumber n:ts -> (makeConst n, ts)
                TkVar x:ts    -> (makeVar x, ts)
                []            -> parseError "Unexpected EOF"
                _             -> parseError "Expected operand"

        parseOperandExponent tokens =
            let (e, remainder) = parseOperand tokens in eatExponents e remainder
            where
                eatExponents e remainder =
                    case remainder of
                        -- Lookahead if TkExpOp is followed by a number.
                        TkExpOp:TkNumber n:ts -> eatExponents (r_exp e n) ts
                        TkExpOp:ts            -> parseError "Expected non-negative number after '^'"
                        TkExponent n:ts       -> eatExponents (r_exp e n) ts
                        _                     -> (e, remainder)

        parseUnaryPlusMinus tokens =
            case tokens of
                TkPlus:ts  -> parseUnaryPlusMinus ts
                TkMinus:ts -> let (e, remainder) = parseUnaryPlusMinus ts in (r_min e, remainder)
                _          -> parseOperandExponent tokens

        parseProduct tokens =
            let (e1, remainder) = parseUnaryPlusMinus tokens
            in  case remainder of
                    TkMultiply:ts -> let (e2, remainder2) = parseProduct ts in (r_mult e1 e2, remainder2)
                    -- These 3 cases allow for a slight ambiguity regarding the difference between binary and unary +/-.
                    -- The interpretation of the resulting syntax trees is the same, so can safely ignore.
                    TkLBracket:_  -> let (e2, remainder2) = parseProduct remainder in (r_mult e1 e2, remainder2)
                    TkNumber _:_  -> let (e2, remainder2) = parseProduct remainder in (r_mult e1 e2, remainder2)
                    TkVar _:_     -> let (e2, remainder2) = parseProduct remainder in (r_mult e1 e2, remainder2)
                    _             -> (e1, remainder)

        parseSum tokens =
            let (e, remainder) = parseProduct tokens
            in  case remainder of
                    TkPlus:ts  -> parseSum' e True ts
                    TkMinus:ts -> parseSum' e False ts
                    _          -> (e, remainder)

        -- Parse this left associatively, because of minus operands.
        parseSum' e1 isPositive tokens =
            let (e2, remainder) = parseProduct tokens
                e               = r_add e1 (if isPositive then e2 else r_min e2)
            in  case remainder of
                    TkPlus:ts  -> parseSum' e True ts
                    TkMinus:ts -> parseSum' e False ts
                    _          -> (e, remainder)

-- Shorthand for 'make polynomial'
mp = parseExpr . lexExpr




-- Tests: create expression from string, transform, show expression.
ut :: IO()
ut = putStrLn $ unitTest 0 0 testExpressions
    where
        unitTest f s x =
            case x of
                []  -> "Run "
                       ++ show (f + s)
                       ++ " tests: "
                       ++ (if (f == 0) then "all successful." else show f ++ " failed.")
                ((input, transform, expected):us)
                    -> let actual = showPoly $ transform $ parseExpr $ lexExpr $ input
                       in  if expected == actual
                              then unitTest f (s+1) us
                              else "Failed test: Expected: \""
                                   ++ expected
                                   ++ "\" Actual: \""
                                   ++ actual
                                   ++ "\" Input: \""
                                   ++ input
                                   ++ "\"\n"
                                   ++ unitTest (f+1) s us

        -- (Input, Transformation function, Expected)
        -- For copy-pasting: ⁰¹²³⁴⁵⁶⁷⁸⁹
        testExpressions =
            [
            -- Exponents
            ("0^1",           id, "0"),
            ("0^5",           id, "0"),
            ("0^5^2",         id, "0"),
            ("2^1",           id, "2"),
            ("2^5",           id, "32"),
            ("2^5^2",         id, "1024"),
            ("x^1",           id, "x"),
            ("x^5",           id, "x⁵"),
            ("x^5^2",         id, "x¹⁰"),
            ("x^5^2^3",       id, "x³⁰"),
            ("(xyz)^5",       id, "x⁵y⁵z⁵"),
            ("(2xy)^5",       id, "32x⁵y⁵"),
            ("2xy^5",         id, "2xy⁵"),
            ("-(2x^2y)^4",    id, "-16x⁸y⁴"),
            ("(2x^2y(-1))^4", id, "16x⁸y⁴"),
            ("(x^5+y)^2",     id, "(x⁵ + y)²"),

            -- Multiplication
            ("(1 (2∙3) 4) 5", id, "120"),
            ("0x",            id, "0"),
            ("1x",            id, "x"),
            ("10x",           id, "10x"),
            ("-x∙3",          id, "-3x"),
            ("y(-xz)∙3",      id, "-3yxz"),

            -- Addition
            ("(1 + (2+3) + 4) + 5", id, "15"),
            ("1 + 0x",              id, "1"),
            ("4 - (5 - 2)",         id, "1"),
            ("4 - 5 - 2",           id, "-3"),
            ("-4 - (5 - 2)",        id, "-7"),
            ("-4 - 5 - 2",          id, "-11"),
            ("--4 -- 5",            id, "9"),
            ("--4 -+-5",            id, "9"),
            ("-+4 +--5",            id, "1"),
            ("-4 - x(5 - 2)",       id, "-3x - 4"),
            ("-y+x",                id, "-y + x"),
            ("1 - x(5 - 2y)",       id, "-x(-2y + 5) + 1"),
            ("1 - x5 - 2y",         id, "-5x - 2y + 1"),

            -- Division
            ("6",      pdiv 1, "6"),
            ("6",      pdiv 2, "6 / 2"), -- not "3"
            ("x",      pdiv 1, "x"),
            ("x",      pdiv 2, "x / 2"),
            ("x^2",    pdiv 2, "x² / 2"),
            ("6",      pexp 2 . pdiv 3, "36 / 9"),
            ("x^2",    pexp 2 . pdiv 3, "x⁴ / 9"),
            ("3x",     pdiv 2, "3x / 2"),
            ("(x⁴)3",  pdiv 2, "3x⁴ / 2"),
            ("-8x⁴",   pdiv 2, "-8x⁴ / 2"),
            ("-8x⁴",   cmult 0 . pdiv 2, "0"),
            ("-8x⁴",   cmult 1 . pdiv 2, "-8x⁴ / 2"),
            ("-8",     xmult . pdiv 2, "-8x / 2"),
            ("-8",     cmult 3 . xmult . pdiv 2, "-24x / 2"),
            ("-8",     cmult 3 . pdiv 2 . xmult, "-24x / 2"),
            ("-8",     pdiv 2 . cmult 3 . xmult, "-24x / 2"),
            ("-8",     cmult 3 . xmult . ymult . pdiv 2, "-24xy / 2"),
            ("18y",    pmult (divPoly (makeVar 'x') 6) . pdiv 5, "18xy / 30"),
            ("-8x",    cadd 3 . pdiv 2, "(-8x + 6) / 2"),
            ("2x",     padds [divPoly (makeVar 'y') 3, divPoly (makeVar 'z') 2] . pdiv 5, "(12x + 10y + 15z) / 30"),
            ("2x",     padds [divPoly (makeConst 1) 4, divPoly (makeVar 'y') 3, divPoly (makeVar 'z') 2] . pdiv 5, "(48x + 40y + 60z + 30) / 120"),
            ("2x",     padds [addPoly [divPoly (makeConst 1) 4, divPoly (makeVar 'y') 3], divPoly (makeVar 'z') 2] . pdiv 5, "(48x + 40y + 60z + 30) / 120"),

            -- Substitution
            ("x+2",        substituteVar 'x' (makeConst 0), "2"),
            ("2x",         substituteVar 'x' (makeConst 0), "0"),
            ("2xy+x-3",    substituteVar 'x' (makeConst 3), "6y"),
            ("(x-3)(x+y)", substituteVar 'x' (makeConst 2), "-(y + 2)"),

            -- Basic parse tests.
            (" 0 ", id, "0"),
            (" x ", id, "x")
            ]

        cadd c p = addPoly [makeConst c, p]
        padds ps q = addPoly (q : ps)
        cmult c p = multPoly [makeConst c, p]
        xmult p = multPoly [makeVar 'x', p]
        ymult p = multPoly [makeVar 'y', p]
        pmult p q = multPoly [p, q]
        pexp = swap expPoly
        pdiv = swap divPoly
