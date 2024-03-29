
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

enumPermuteGraphEdgeMappings :: (Enum a, Ord a, Show a) => UndirectedGraph a -> [(String, GraphEdge a -> GraphEdge a)]
enumPermuteGraphEdgeMappings graph@(UndirectedGraph vmin _) =
    map (fmap (permuteEdge . permuteVertex)) $ namedPermutations $ enumGraphVertices graph
    where
        permuteVertex :: Enum b => [GraphVertex a] -> GraphVertex b -> GraphVertex a
        permuteVertex vs (GraphVertex n) = vs !! (fromEnum n - fromEnum vmin)

        permuteEdge :: Ord a => (GraphVertex b -> GraphVertex a) -> GraphEdge b -> GraphEdge a
        permuteEdge permuteVertex (GraphEdge v w) =
            case (permuteVertex v, permuteVertex w) of
                (v', w') | v' <= w'  -> GraphEdge v' w'
                         | otherwise -> GraphEdge w' v'

graphPolyaGroup :: (Ord a, Enum a, Num a, Show a) => a -> PolyaGroup (GraphEdge a)
graphPolyaGroup n =
    makePolyaGroup (enumUndirectedNonReflexiveGraphEdges ug) (enumPermuteGraphEdgeMappings ug)
    where
        ug = undirectedGraph 0 (n - 1)

-- Not only permutes a list but also generates display names for all permutations.
namedPermutations :: Show a => [a] -> [(String, [a])]
namedPermutations xs = map addName $ permutations xs
    where
        addName permutation = (show xs ++ " -> " ++ show permutation, permutation)




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

enumBoardSymmetryOperations :: (Ord a, Num a) => SquareBoard a -> [(String, SquareBoardCoordinate a -> SquareBoardCoordinate a)]
enumBoardSymmetryOperations (SquareBoard n)
    | n > 1     = [("id",                 symmetryId n),
                   ("vertical flip",      vFlip n),
                   ("horizontal flip",    hFlip n),
                   ("slash flip",         slashFlip n),
                   ("backslash flip",     backSlashFlip n),
                   ("rotate 90 degrees",  rotate90 n),
                   ("rotate 180 degrees", rotate180 n),
                   ("rotate 270 degrees", rotate270 n)]
    | otherwise = [("id",                 symmetryId n)]  -- trivial group

sqBoardPolyaGroup :: (Ord a, Enum a, Num a) => a -> PolyaGroup (SquareBoardCoordinate a)
sqBoardPolyaGroup n =
    makePolyaGroup [SquareBoardCoordinate x y | x <- [0..n - 1], y <- [0..n - 1]] (enumBoardSymmetryOperations (squareBoard n))




-- Shorthand functions.

-- 'make polynomial'
-- > substituteVar 'x' (makeConst 3) (mp "x^2 + 1")
-- 10
mp = parseExpr . lexExpr

polyOne :: Polynomial Integer Char
polyOne = makeConst 1

substVarWith1 v p = substituteVar v polyOne p

substAllVarsWith1 p = substitute p (\v -> polyOne)




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
                                   ++ replaceOddChars expected
                                   ++ "\" Actual: \""
                                   ++ replaceOddChars actual
                                   ++ "\" Input: \""
                                   ++ replaceOddChars input
                                   ++ "\"\n"
                                   ++ unitTest (f+1) s us

        -- (Input, Transformation function, Expected)
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
            ("(-2x^2y)^4",    id, "16x⁸y⁴"),
            ("(2x^2y(-1))^4", id, "16x⁸y⁴"),
            ("(x^5+y)^2",     id, "(x⁵ + y)²"),
            ("(x^5+y)^2^3",   id, "(x⁵ + y)⁶"),

            -- Multiplication
            ("(1 (2∙3) 4) 5", id, "120"),
            ("0x",            id, "0"),
            ("1x",            id, "x"),
            ("10x",           id, "10x"),
            ("-x∙3",          id, "-3x"),
            ("y(-xz)∙3",      id, "-3xyz"),

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
            ("-y+x",                id, "x - y"),
            ("1 - x(5 - 2y)",       id, "-x(-2y + 5) + 1"),
            ("1 - x5 - 2y",         id, "-5x - 2y + 1"),

            -- Division
            ("6",      pdiv 1, "6"),
            ("6",      pdiv 2, "3"),
            ("x",      pdiv 1, "x"),
            ("x",      pdiv 2, "x / 2"),
            ("x^2",    pdiv 2, "x² / 2"),
            ("6",      pexp 2 . pdiv 3, "4"),
            ("x^2",    pexp 2 . pdiv 3, "x⁴ / 9"),
            ("3x",     pdiv 2, "3x / 2"),
            ("(x⁴)3",  pdiv 2, "3x⁴ / 2"),
            ("-8x⁴",   pdiv 2, "-4x⁴"),
            ("-8x⁴",   cmult 0 . pdiv 2, "0"),
            ("-8x⁴",   cmult 1 . pdiv 2, "-4x⁴"),
            ("-8",     xmult . pdiv 2, "-4x"),
            ("-8",     cmult 3 . xmult . pdiv 2, "-12x"),
            ("-8",     cmult 3 . pdiv 2 . xmult, "-12x"),
            ("-8",     pdiv 2 . cmult 3 . xmult, "-12x"),
            ("-8",     cmult 3 . xmult . ymult . pdiv 2, "-12xy"),
            ("18y",    pmult (divPoly (makeVar 'x') 6) . pdiv 5, "3xy / 5"),
            ("-8x",    cadd 3 . pdiv 2, "-4x + 3"),
            ("-8x+2y", cadd 3 . pdiv 2, "-4x + y + 3"),
            ("-8x+y",  cadd 3 . pdiv 2, "(-8x + y + 6) / 2"),
            ("2x",     padds [divPoly (makeVar 'y') 3, divPoly (makeVar 'z') 2] . pdiv 5, "(12x + 10y + 15z) / 30"),
            ("2x",     padds [divPoly (makeConst 1) 4, divPoly (makeVar 'y') 3, divPoly (makeVar 'z') 2] . pdiv 5, "(24x + 20y + 30z + 15) / 60"),
            ("2x",     padds [addPoly [divPoly (makeConst 1) 4, divPoly (makeVar 'y') 3], divPoly (makeVar 'z') 2] . pdiv 5, "(24x + 20y + 30z + 15) / 60"),

            -- Choose least common multiple for a divisor,
            -- both when addPoly and divPoly are the last operators to be applied.
            ("0", buildDivAddPoly1, "(105a + 30b + 70c + 21d + 210e + 35f) / 210"),
            ("0", buildDivAddPoly2, "(105a + 30b + 70c + 21d + 210e + 35f) / 210"),
            ("0", buildDivAddPoly3, "(2a + 7b + 3c + 10d + e + 6f) / 6350400"),

            -- Substitution
            ("x+2",        substituteVar 'x' (makeConst 0), "2"),
            ("2x",         substituteVar 'x' (makeConst 0), "0"),
            ("2xy+x-3",    substituteVar 'x' (makeConst 3), "6y"),
            ("(x-3)(x+y)", substituteVar 'x' (makeConst 2), "-y - 2"),

            -- Distribution of constants over sums
            ("3(x + 2)",          id,     "3x + 6"),
            ("3(x + 2)",          pdiv 2, "(3x + 6) / 2"),
            ("3(x + y)",          id,     "3x + 3y"),
            ("-(x - y)",          id,     "-x + y"),
            ("-(2x - 2y)",        id,     "-2x + 2y"),
            ("-(2x - (y-z))",     id,     "-2x + y - z"),
            ("-(2x - (-y+z))",    id,     "-2x - y + z"),
            ("-(2x + (-y+z))",    id,     "-2x + y - z"),
            ("-2(-2x + 3(y+2z))", id,     "4x - 6y - 12z"),
            ("-2(-2x + y(y+2z))", id,     "4x - 2y(y + 2z)"),
            ("-y(-2x - 3(y-2z))", id,     "-y(-2x - 3y + 6z)"),

            -- Ordering and grouping
            ("3x + xx",               id, "x² + 3x"),
            ("y + x",                 id, "x + y"),
            ("yx",                    id, "xy"),
            ("1 + y^2 + x",           id, "x + y² + 1"),
            ("xx^2x",                 id, "x⁴"),
            ("yx^2xy^2x",             id, "x⁴y³"),
            ("(y+x)(x+y)",            id, "(x + y)²"),
            ("(y+x)^2(x+y)",          id, "(x + y)³"),
            ("(y+x^2)(x^2+y)",        id, "(x² + y)²"),
            ("(2x+1)^2 * (3x+1)^2",   id, "(3x + 1)²(2x + 1)²"),
            ("(2x+1)^2 + (3x+1)^2",   id, "(3x + 1)² + (2x + 1)²"),
            -- Expand scalar * add expressions.
            ("3(y+x)+4(x+y)",          id, "7x + 7y"),
            ("6(y+x)+1(x+y)",          id, "7x + 7y"),
            ("6(z+x)+y+z+x",           id, "7x + y + 7z"),
            ("6(z+x)+2(x+y)+y+z+x",    id, "9x + 3y + 7z"),
            ("4y²-7(x-y)+x²-1",        id, "x² - 7x + 4y² + 7y - 1"),
            -- Complex grouping.
            ("(x^2+y)^2 + 2(x+y^2)^3", id, "(x² + y)² + 2(x + y²)³"),
            ("2(x+y^2)^3 + (x^2+y)^2", id, "(x² + y)² + 2(x + y²)³"),
            ("(x+y^2)^3 + 2(x^2+y)^2", id, "2(x² + y)² + (x + y²)³"),
            ("(x^2+y)^2 + 2(x+y^2)^2", id, "(x² + y)² + 2(x + y²)²"),
            ("2(x^2+y)^2 + (x+y^2)^2", id, "2(x² + y)² + (x + y²)²"),
            ("y(x^2+z) + y^2",         id, "y² + y(x² + z)"),
            ("y^2 + (z+x^2)y",         id, "y² + y(x² + z)"),
            ("(x+1)^2 * (x+2)^2",      id, "(x + 2)²(x + 1)²"),
            ("(x+1)^2 * (2+x)^2",      id, "(x + 2)²(x + 1)²"),
            ("(x+1)^2 * (1+x)^2",      id, "(x + 1)⁴"),
            ("x(x^2+1) + x(1+x^3)",    id, "x(x³ + 1) + x(x² + 1)"),
            ("(4x²+6)³ + 2(2*3+5x*x-x²)(2³+(x(3x+x))-0y-2)²", id, "3(4x² + 6)³"),

            -- Expand
            ("(a+b)²",               expand, "a² + 2ab + b²"),
            ("(a + b)(a - b)",       expand, "a² - b²"),
            ("(a - b + c)(a + b)",   expand, "a² + ac - b² + bc"),
            ("(x + 4)(x - 2)",       expand, "x² + 2x - 8"),
            ("(a^2+2ab+b^2)(2a-b)",  expand, "2a³ + 3a²b - b³"),
            ("(2a^2+2ab+b^2)(2a-b)", expand, "4a³ + 2a²b - b³"),
            ("(a+1)^2",              expand, "a² + 2a + 1"),
            ("(1-a)^3",              expand, "-a³ + 3a² - 3a + 1"),
            -- The characteristic polynomial for all tic-tac-toe symmetries:
            ("(e+x+c)⁹ + 4(e+x+c)³(e²+x²+c²)³ + 2(e+x+c)(e⁴+x⁴+c⁴)² + (e+x+c)(e²+x²+c²)⁴",
             expand . pdiv 8,
             "c⁹ + 3c⁸e + 3c⁸x + 8c⁷e² + 12c⁷ex + 8c⁷x² + 16c⁶e³ + 38c⁶e²x + 38c⁶ex² + 16c⁶x³ + 23c⁵e⁴ + 72c⁵e³x + 108c⁵e²x² + 72c⁵ex³ + 23c⁵x⁴ + 23c⁴e⁵ + 89c⁴e⁴x + 174c⁴e³x² + 174c⁴e²x³ + 89c⁴ex⁴ + 23c⁴x⁵ + 16c³e⁶ + 72c³e⁵x + 174c³e⁴x² + 228c³e³x³ + 174c³e²x⁴ + 72c³ex⁵ + 16c³x⁶ + 8c²e⁷ + 38c²e⁶x + 108c²e⁵x² + 174c²e⁴x³ + 174c²e³x⁴ + 108c²e²x⁵ + 38c²ex⁶ + 8c²x⁷ + 3ce⁸ + 12ce⁷x + 38ce⁶x² + 72ce⁵x³ + 89ce⁴x⁴ + 72ce³x⁵ + 38ce²x⁶ + 12cex⁷ + 3cx⁸ + e⁹ + 3e⁸x + 8e⁷x² + 16e⁶x³ + 23e⁵x⁴ + 23e⁴x⁵ + 16e³x⁶ + 8e²x⁷ + 3ex⁸ + x⁹"),

            -- Coefficient (select only that part of the expanded polynomial
            -- with a particular set of variables raised to some power)
            ("-2",          coefficient [],                "-2"),
            ("-2",          coefficient [('a',1)],         "0"),
            ("(a+b)²",      coefficient [],                "0"),
            ("(a+b)²",      coefficient [('a',1)],         "0"),
            ("(a+b)²",      coefficient [('b',1)],         "0"),
            ("(a+b)²",      coefficient [('a',1),('b',1)], "2ab"),
            ("(a+b)²",      coefficient [('a',2)],         "a²"),
            ("(a+b)²",      coefficient [('b',2)],         "b²"),
            ("(a+3)⁴",      coefficient [],                "81"),
            ("(a+3)⁴",      coefficient [('a',1)],         "108a"),
            ("(a+3)⁴",      coefficient [('a',2)],         "54a²"),
            ("(a+3)⁴",      coefficient [('a',3)],         "12a³"),
            ("(a+3)⁴",      coefficient [('a',4)],         "a⁴"),
            -- Ignore zero exponents.
            ("(a+3)⁴",      coefficient [('a',0)],         "81"),
            -- Correct handling of minuses which can make coefficients disappear.
            ("(a+b)(a-b)",  coefficient [],                "0"),
            ("(a+b)(a-b)",  coefficient [('a',1)],         "0"),
            ("(a+b)(a-b)",  coefficient [('b',1)],         "0"),
            ("(a+b)(a-b)",  coefficient [('a',1),('b',1)], "0"),
            ("(a+b)(a-b)",  coefficient [('a',2)],         "a²"),
            ("(a+b)(a-b)",  coefficient [('b',2)],         "-b²"),
            ("-(a+b)(a-b)", coefficient [('b',2)],         "b²"),
            -- Number of different positions with a single king on a chess board.
            ("(k+1)⁶⁴",     coefficient [('k',1)],         "64k"),
            -- Number of different positions with a single king on a chess board
            -- taking symmetries into account.
            ("2(k⁴+1)¹⁶ + 3(k²+1)³² + 2(k²+1)²⁸(k+1)⁸ + (k+1)⁶⁴", coefficient [('k',1)] . pdiv 8, "10k"),

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
        pexp = flip expPoly
        pdiv = flip divPoly

        -- "a/2 + b/7 + c/3 + d/10 + e + f/6"
        buildDivAddPoly1 _ =
            addPoly [
                divPoly (makeVar 'a') 2,
                divPoly (makeVar 'b') 7,
                divPoly (makeVar 'c') 3,
                divPoly (makeVar 'd') 10,
                divPoly (makeVar 'e') 1,
                divPoly (makeVar 'f') 6
            ]

        -- "(1260a + 360b + 840c + 252d + 2520e + 420f) / 2520"
        -- "( 105a +  30b +  70c +  21d +  210e +  35f) / 210"
        buildDivAddPoly2 _ =
            divPoly (addPoly [
                multPoly [makeVar 'a', makeConst 1260],
                multPoly [makeVar 'b', makeConst 360],
                multPoly [makeVar 'c', makeConst 840],
                multPoly [makeVar 'd', makeConst 252],
                multPoly [makeVar 'e', makeConst 2520],
                multPoly [makeVar 'f', makeConst 420]
            ]) 2520

        -- "(a/1260 + b/360 + c/840 + d/252 + e/2520 + f/420) / 2520"
        -- "(2a + 7b + 3c + 10d + e + 6f) / 6350400"
        buildDivAddPoly3 _ =
           divPoly (addPoly [
               divPoly (makeVar 'a') 1260,
               divPoly (makeVar 'b') 360,
               divPoly (makeVar 'c') 840,
               divPoly (makeVar 'd') 252,
               divPoly (makeVar 'e') 2520,
               divPoly (makeVar 'f') 420
           ]) 2520

        -- Because WinGHCi cannot show these characters.
        replaceOddChars [] = []
        replaceOddChars (x:xs) = replaceOddChar x ++ replaceOddChars xs

        replaceOddChar '⁰' = "^0"
        replaceOddChar '⁴' = "^4"
        replaceOddChar '⁵' = "^5"
        replaceOddChar '⁶' = "^6"
        replaceOddChar '⁷' = "^7"
        replaceOddChar '⁸' = "^8"
        replaceOddChar '⁹' = "^9"

        replaceOddChar '∙' = "*"
        replaceOddChar x   = [x]
