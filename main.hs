
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
                        TkExpOp:TkNumber n:ts -> eatExponents (expPoly e n) ts
                        TkExpOp:ts            -> parseError "Expected non-negative number after '^'"
                        TkExponent n:ts       -> eatExponents (expPoly e n) ts
                        _                     -> (e, remainder)

        parseUnaryPlusMinus tokens =
            case tokens of
                TkPlus:ts  -> parseUnaryPlusMinus ts
                TkMinus:ts -> let (e, remainder) = parseUnaryPlusMinus ts in (multPoly [makeConst (-1), e], remainder)
                _          -> parseOperandExponent tokens

        parseProduct tokens =
            let (e1, remainder) = parseUnaryPlusMinus tokens
            in  case remainder of
                    TkMultiply:ts -> let (e2, remainder2) = parseProduct ts in (multPoly [e1, e2], remainder2)
                    -- These 3 cases allow for a slight ambiguity regarding the difference between binary and unary +/-.
                    -- The interpretation of the resulting syntax trees is the same, so can safely ignore.
                    TkLBracket:_  -> let (e2, remainder2) = parseProduct remainder in (multPoly [e1, e2], remainder2)
                    TkNumber _:_  -> let (e2, remainder2) = parseProduct remainder in (multPoly [e1, e2], remainder2)
                    TkVar _:_     -> let (e2, remainder2) = parseProduct remainder in (multPoly [e1, e2], remainder2)
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
                e               = addPoly [e1, if isPositive then e2 else multPoly [makeConst (-1), e2]]
            in  case remainder of
                    TkPlus:ts  -> parseSum' e True ts
                    TkMinus:ts -> parseSum' e False ts
                    _          -> (e, remainder)

-- Shorthand for 'make polynomial'
mp = parseExpr . lexExpr
