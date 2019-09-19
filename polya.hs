module Eutherion.Polya (

       PolyaGroup,
       makePolyaGroup,
       cayleyTable,
       orbit,
       characteristic

       ) where

import Data.Array
import Data.List

import Eutherion.Utilities
import Eutherion.Combinatorics
import Eutherion.CommutativeRing
import Eutherion.Polynomial

-- Encapsulates a list of elements, and a list of symmetry operations acting on those elements.
data PolyaGroup a = PolyaGroup [a] [(String, a -> a)]

makePolyaGroup :: [a] -> [(String, a -> a)] -> PolyaGroup a
makePolyaGroup slots fns = PolyaGroup slots fns

instance Show (PolyaGroup a) where
    show (PolyaGroup slots fns) =  "Number of slots: " ++ show (length slots) ++ "\n"
                                ++ "Number of symmetries: " ++ show (length fns)

-- Builds a Cayley table of a symmetry group, if it is indeed a group. Errors otherwise.
cayleyTable :: Eq a => PolyaGroup a -> CayleyTable
cayleyTable (PolyaGroup slots namedFns) =
    -- Apply each symmetry operation on each element in 'slots'.
    let n            = length namedFns
        infos        = map (getInfo slots) applied
        identity     = case findIndex fst3 infos of
                           Just x  -> x
                           Nothing -> error "No identity element found"
        multTable    = listToZeroIndexedArray [array (0, n - 1) t | t <- map snd3 infos]
        inverseTable = listToZeroIndexedArray $ map thd3 infos
    in  buildCayleyTableOptimistic n identity multTable inverseTable
    where
        listToZeroIndexedArray xs = listArray (0, length xs - 1) xs

        -- For readability. Applies all symmetry functions on all slots.
        applyNamedFnsOnAllSlots slots = map (fmap (flip map slots))

        -- Applies each symmetry operation on each element in 'slots'.
        applied = map assertClosed $ applyNamedFnsOnAllSlots slots namedFns
            where
                -- Checks if each function maps each slot onto another slot from the list.
                assertClosed namedSlots@(fnName, slots') =
                    case all (flip elem slots) slots' of
                        True -> namedSlots
                        _    -> error ("Symmetry '" ++ fnName ++ "' does not map onto the range of slots.")

        -- Whether it's the identity element, multiplication table entry, inverse element index.
        getInfo slots (firstFnName, slots') =
            (isIdentity, thirdFnIndexes, inverse)
            where
                isIdentity = slots == slots'
                secondFnApplied = applyNamedFnsOnAllSlots slots' namedFns
                thirdFnIndexes =
                    [(j, findThirdFn j slots'') | (j, slots'') <- zip [0..] (map snd secondFnApplied)]
                    where
                        findThirdFn j slots'' =
                            case elemIndex slots'' (map snd applied) of
                                Just x  -> x
                                Nothing -> error ("Function " ++ show j ++ " applied after function '" ++ firstFnName ++ "' is either not associative, or not closed")
                inverse =
                    case elemIndex slots (map snd secondFnApplied) of
                        Just x  -> x
                        Nothing -> error ("Symmetry '" ++ firstFnName ++ "' has no inverse.")


-- Returns the orbit of a value 'x' under function 'f',
-- i.e. the list of values which is the result of iterating the function 'f'
-- on 'x', until 'x' is returned again.
-- > orbit ((swap mod 7) . (*3)) 1
-- [1,3,2,6,4,5]
orbit :: Eq a => (a -> a) -> a -> [a]
orbit f x = x : (takeWhile (/= x) $ tail $ iterate f x)


-- Returns the characteristic polynomial for a Pólya group and a number of choices represented by characters.
-- Example usage:
-- > characteristic (sqBoardPolyaGroup 3) "exo"
-- (2(e^4 + o^4 + x^4)^2(e + o + x) + (e^2 + o^2 + x^2)^4(e + o + x) + 4(e^2 + o^2 + x^2)^3(e + o + x)^3 + (e + o + x)^9) / 8
-- > expand $ characteristic (sqBoardPolyaGroup 3) "exo"
-- e^9 + 3e^8o + 3e^8x + 8e^7o^2 + 12e^7ox + 8e^7x^2 + 16e^6o^3 + 38e^6o^2x + 38e^6ox^2 + 16e^6x^3 + 23e^5o^4 + 72e^5o^3x + 108e^5o^2x^2 + 72e^5ox^3 + 23e^5x^4 + 23e^4o^5 + 89e^4o^4x + 174e^4o^3x^2 + 174e^4o^2x^3 + 89e^4ox^4 + 23e^4x^5 + 16e^3o^6 + 72e^3o^5x + 174e^3o^4x^2 + 228e^3o^3x^3 + 174e^3o^2x^4 + 72e^3ox^5 + 16e^3x^6 + 8e^2o^7 + 38e^2o^6x + 108e^2o^5x^2 + 174e^2o^4x^3 + 174e^2o^3x^4 + 108e^2o^2x^5 + 38e^2ox^6 + 8e^2x^7 + 3eo^8 + 12eo^7x + 38eo^6x^2 + 72eo^5x^3 + 89eo^4x^4 + 72eo^3x^5 + 38eo^2x^6 + 12eox^7 + 3ex^8 + o^9 + 3o^8x + 8o^7x^2 + 16o^6x^3 + 23o^5x^4 + 23o^4x^5 + 16o^3x^6 + 8o^2x^7 + 3ox^8 + x^9
-- > expand $ characteristic (graphPolyaGroup 4) ".x"
-- .^6 + .^5x + 2.^4x^2 + 3.^3x^3 + 2.^2x^4 + .x^5 + x^6
characteristic :: (Eq a, Ord b) => PolyaGroup a -> [b] -> Polynomial Integer b
characteristic (PolyaGroup slots symmetries) cs =
    divPoly (genExpression symmetries) (toInteger $ length symmetries)
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
        orbitLengths symmetry = removeSharedOrbits $ sort $ map (length . orbit symmetry) slots

        removeSharedOrbits os =
            case os of
                []   -> []
                o:os -> o : removeSharedOrbits (drop (o - 1) os)

        -- Generates e.g. e^4 + o^4 + x^4
        selectOneValue orbitLength = addPoly [expPoly (makeVar c) (toInteger orbitLength) | c <- cs]

        genExpression = addPoly . map (multPoly . map selectOneValue . orbitLengths . snd)
