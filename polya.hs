module Eutherion.Polya (

       PolyaGroup,
       makePolyaGroup,
       pgSlots,
       pgSymmetries,

       cayleyTable,

       orbit,
       Cycle(..),
       cycleIndex,
       characteristic

       ) where

import Data.Array
import Data.List

import Eutherion.Utilities
import Eutherion.Combinatorics
import Eutherion.Polynomial

-- Encapsulates a list of elements, and a list of named symmetry operations acting on those elements.
data PolyaGroup a = PolyaGroup {
    pgSlots      :: Array Int a,
    pgSymmetries :: Array Int (String, a -> a)
}

-- (Private)
listToZeroIndexedArray xs = listArray (0, length xs - 1) xs

-- (Private)
lengthZeroBasedIndexArray = (+1) . snd . bounds

makePolyaGroup :: [a] -> [(String, a -> a)] -> PolyaGroup a
makePolyaGroup slots symmetries = PolyaGroup (listToZeroIndexedArray slots) (listToZeroIndexedArray symmetries)

instance Show (PolyaGroup a) where
    show (PolyaGroup slots symmetries) =  "Number of slots: " ++ show (lengthZeroBasedIndexArray slots) ++ "\n"
                                       ++ "Number of symmetries: " ++ show (lengthZeroBasedIndexArray symmetries)

-- Builds a Cayley table of a symmetry group, if it is indeed a group. Errors otherwise.
cayleyTable :: Eq a => PolyaGroup a -> CayleyTable
cayleyTable (PolyaGroup slots symmetries) =
    -- Apply each symmetry operation on each element in 'slots'.
    let infos        = fmap getInfo applied
        identity     = case findArrayIndexBy fst3 infos of
                           Just x  -> x
                           Nothing -> error "No identity element found"
        multTable    = fmap snd3 infos
        inverseTable = fmap thd3 infos
    in  buildCayleyTableOptimistic (lengthZeroBasedIndexArray symmetries) identity multTable inverseTable
    where
        -- For readability. Applies all symmetry functions on all slots.
        applySymmetriesToAllSlots slots = fmap (fmap (flip fmap slots))

        -- Applies each symmetry operation on each element in 'slots'.
        applied = fmap assertClosed $ applySymmetriesToAllSlots slots symmetries
            where
                -- Checks if each function maps each slot onto another slot from the list.
                assertClosed namedSlots@(fnName, slots') =
                    case all (flip elem slots) slots' of
                        True -> namedSlots
                        _    -> error ("Symmetry '" ++ fnName ++ "' does not map onto the range of slots.")

        -- Whether it's the identity element, multiplication table entry, inverse element index.
        getInfo (firstFnName, slots') =
            (isIdentity, thirdFnIndexes, inverse)
            where
                isIdentity = slots == slots'
                secondFnApplied = applySymmetriesToAllSlots slots' symmetries
                thirdFnIndexes = fmap findThirdFn secondFnApplied
                    where
                        findThirdFn (secondFnName, slots'') =
                            case findArrayIndex slots'' $ fmap snd applied of
                                Just x  -> x
                                Nothing -> error ("The symmetry group is not closed. Symmetry '"
                                                  ++ secondFnName
                                                  ++ "' applied after '"
                                                  ++ firstFnName
                                                  ++ "' is not equal to any other symmetry.")
                inverse =
                    case findArrayIndex slots $ fmap snd secondFnApplied of
                        Just x  -> x
                        Nothing -> error ("Symmetry '" ++ firstFnName ++ "' has no inverse.")


-- Returns the orbit of a value 'x' under function 'f',
-- i.e. the list of values which is the result of iterating the function 'f'
-- on 'x', until 'x' is returned again.
-- > orbit ((swap mod 7) . (*3)) 1
-- [1,3,2,6,4,5]
orbit :: Eq a => (a -> a) -> a -> [a]
orbit f x = x : (takeWhile (/= x) $ tail $ iterate f x)

data Cycle = Cycle { cycleSize :: Integer } deriving (Eq, Show)

-- Reverse order because polynomials are sorted from largest to smallest contributor.
instance Ord Cycle where
    compare (Cycle m) (Cycle n) = compare n m

instance ShowablePolynomialVariable Cycle where
    showVar (Cycle n) = "a[" ++ show n ++ "]"

-- Returns the cycle index of a Pólya group.
-- Example usage:
-- > cycleIndex (sqBoardPolyaGroup 3)
-- (2a[4]^2a[1] + a[2]^4a[1] + 4a[2]^3a[1]^3 + a[1]^9) / 8
-- > cycleIndex (graphPolyaGroup 4)
-- (6a[4]a[2] + 8a[3]^2 + 9a[2]^2a[1]^2 + a[1]^6) / 24
cycleIndex :: Eq a => PolyaGroup a -> Polynomial Integer Cycle
cycleIndex (PolyaGroup slots symmetries) =
    divPoly (genExpression symmetries) (toInteger $ lengthZeroBasedIndexArray symmetries)
    where
        -- For sqBoardPolyaGroup 3, '(removeSharedOrbits . sort . orbitLengths) symmetry'
        -- has these values for all 8 symmetries:
        -- [1,1,1,1,1,1,1,1,1]
        -- [1,1,1,2,2,2]
        -- [1,1,1,2,2,2]
        -- [1,1,1,2,2,2]
        -- [1,1,1,2,2,2]
        -- [1,4,4]
        -- [1,2,2,2,2]
        -- [1,4,4]
        orbitLengths symmetry = removeSharedOrbits $ sort $ elems $ fmap (length . orbit symmetry) slots

        removeSharedOrbits os =
            case os of
                []   -> []
                o:os -> o : removeSharedOrbits (drop (o - 1) os)

        selectOneValue orbitLength = makeVar (Cycle (toInteger orbitLength))

        genExpression = addPoly . fmap (multPoly . map selectOneValue . orbitLengths . snd)

-- Returns the characteristic polynomial for a Pólya group and a number of choices represented by distinct variables.
-- Example usage:
-- > characteristic (sqBoardPolyaGroup 3) "exo"
-- (2(e^4 + o^4 + x^4)^2(e + o + x) + (e^2 + o^2 + x^2)^4(e + o + x) + 4(e^2 + o^2 + x^2)^3(e + o + x)^3 + (e + o + x)^9) / 8
-- > expand $ characteristic (sqBoardPolyaGroup 3) "exo"
-- e^9 + 3e^8o + 3e^8x + 8e^7o^2 + 12e^7ox + 8e^7x^2 + 16e^6o^3 + 38e^6o^2x + 38e^6ox^2 + 16e^6x^3 + 23e^5o^4 + 72e^5o^3x + 108e^5o^2x^2 + 72e^5ox^3 + 23e^5x^4 + 23e^4o^5 + 89e^4o^4x + 174e^4o^3x^2 + 174e^4o^2x^3 + 89e^4ox^4 + 23e^4x^5 + 16e^3o^6 + 72e^3o^5x + 174e^3o^4x^2 + 228e^3o^3x^3 + 174e^3o^2x^4 + 72e^3ox^5 + 16e^3x^6 + 8e^2o^7 + 38e^2o^6x + 108e^2o^5x^2 + 174e^2o^4x^3 + 174e^2o^3x^4 + 108e^2o^2x^5 + 38e^2ox^6 + 8e^2x^7 + 3eo^8 + 12eo^7x + 38eo^6x^2 + 72eo^5x^3 + 89eo^4x^4 + 72eo^3x^5 + 38eo^2x^6 + 12eox^7 + 3ex^8 + o^9 + 3o^8x + 8o^7x^2 + 16o^6x^3 + 23o^5x^4 + 23o^4x^5 + 16o^3x^6 + 8o^2x^7 + 3ox^8 + x^9
-- > expand $ characteristic (graphPolyaGroup 4) ".x"
-- .^6 + .^5x + 2.^4x^2 + 3.^3x^3 + 2.^2x^4 + .x^5 + x^6
characteristic :: (Eq a, Ord b, Foldable f, Functor f) => PolyaGroup a -> f b -> Polynomial Integer b
characteristic pg vs = substitute (cycleIndex pg) substituteCycleSize
    where
        -- Replaces e.g. a[4] with e^4 + o^4 + x^4.
        substituteCycleSize (Cycle n) = addPoly $ fmap expVar vs
            where
                expVar v = expPoly (makeVar v) n
