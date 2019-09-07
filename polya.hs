module Eutherion.Polya (

       PolyaGroup,
       makePolyaGroup,
       cayleyTable,
       characteristic

       ) where

import Data.Array
import Data.List

import Eutherion.Utilities
import Eutherion.Combinatorics
import Eutherion.CommutativeRing
import Eutherion.Polynomial

-- Encapsulates a list of elements, and a list of symmetry operations acting on those elements.
data PolyaGroup a = PolyaGroup [a] [a -> a]

makePolyaGroup :: [a] -> [a -> a] -> PolyaGroup a
makePolyaGroup slots fns = PolyaGroup slots fns

instance Show (PolyaGroup a) where
    show (PolyaGroup slots fns) =  "Number of slots: " ++ show (length slots) ++ "\n"
                                ++ "Number of symmetries: " ++ show (length fns)

-- Builds a Cayley table of a symmetry group, if it is indeed a group. Errors otherwise.
cayleyTable :: Eq a => PolyaGroup a -> CayleyTable
cayleyTable (PolyaGroup slots fns) =
    -- Apply each symmetry operation on each element in 'slots'.
    let n            = length fns
        infos        = [getInfo slots slots' | slots' <- zip [0..] applied]
        identity     = case findIndex fst3 infos of
                           Just x  -> x
                           Nothing -> error "No identity element found"
        multTable    = listToSimpleArray [array (0, n - 1) t | t <- map snd3 infos]
        inverseTable = listToSimpleArray $ map thd3 infos
    in  buildCayleyTableOptimistic n identity multTable inverseTable
    where
        applied = assertNoDuplicates $ [assertClosed slots (zip [0..] [fn slot | slot <- slots]) | fn <- fns]

        assertClosed slots slots' =
            let resultInSlots = [(i, elem x slots) | (i, x) <- slots']
            in  case all snd resultInSlots of
                    True -> map snd slots'
                    _    -> error ("Function(s) " ++ intercalate ", " [show i | (i, ok) <- resultInSlots, not ok] ++ " violate(s) the binary algebra condition")

        assertNoDuplicates slotsList =
            -- Not the most efficient implementation, but is fortunately only executed once.
            let equalSlotLists = [length [candidate | candidate <- slotsList, candidate == slots'] | slots' <- slotsList]
            in  case all ((==) 1) equalSlotLists of
                    True -> slotsList
                    _    -> error ("There are duplicate functions.")

        -- Whether it's the identity element, multiplication table entry, inverse element index.
        getInfo slots (i, slots') =
            (isIdentity, thirdFnIndexes, inverse)
            where
                isIdentity = slots == slots'
                secondFnApplied = [[fn slot' | slot' <- slots'] | fn <- fns]
                thirdFnIndexes =
                    [(j, findThirdFn j slots'') | (j, slots'') <- zip [0..] secondFnApplied]
                    where
                        findThirdFn j slots'' =
                            case elemIndex slots'' applied of
                                Just x  -> x
                                Nothing -> error ("Function " ++ show j ++ " applied after function " ++ show i ++ " is either not associative, or not closed")
                inverse =
                    case elemIndex slots secondFnApplied of
                        Just x  -> x
                        Nothing -> error ("Function " ++ show i ++ " has no inverse.")


-- Returns the characteristic polynomial for a Pólya group and a number of choices represented by characters.
-- Example usage:
-- > expand $ characteristic (sqBoardPolyaGroup 3) "exo"
-- e^9 + 3e^8o + 3e^8x + 8e^7o^2 + 12e^7ox + 8e^7x^2 + 16e^6o^3 + 38e^6o^2x + 38e^6ox^2 + 16e^6x^3 + 23e^5o^4 + 72e^5o^3x + 108e^5o^2x^2 + 72e^5ox^3 + 23e^5x^4 + 23e^4o^5 + 89e^4o^4x + 174e^4o^3x^2 + 174e^4o^2x^3 + 89e^4ox^4 + 23e^4x^5 + 16e^3o^6 + 72e^3o^5x + 174e^3o^4x^2 + 228e^3o^3x^3 + 174e^3o^2x^4 + 72e^3ox^5 + 16e^3x^6 + 8e^2o^7 + 38e^2o^6x + 108e^2o^5x^2 + 174e^2o^4x^3 + 174e^2o^3x^4 + 108e^2o^2x^5 + 38e^2ox^6 + 8e^2x^7 + 3eo^8 + 12eo^7x + 38eo^6x^2 + 72eo^5x^3 + 89eo^4x^4 + 72eo^3x^5 + 38eo^2x^6 + 12eox^7 + 3ex^8 + o^9 + 3o^8x + 8o^7x^2 + 16o^6x^3 + 23o^5x^4 + 23o^4x^5 + 16o^3x^6 + 8o^2x^7 + 3ox^8 + x^9
-- > expand $ characteristic (graphPolyaGroup 4) ".x"
-- .^6 + .^5x + 2.^4x^2 + 3.^3x^3 + 2.^2x^4 + .x^5 + x^6
characteristic :: (Eq a, Ord b) => PolyaGroup a -> [b] -> Polynomial Integer b
characteristic (PolyaGroup slots symmetries) cs =
    divPoly (genExpression cs $ choiceFunction orbitLengths) (toInteger $ length symmetries)
    where
        -- For sqBoardPolyaGroup 3, orbitLengths has this value:
        -- [[1,1,1,1,1,1,1,1,1],[2,2,2,1,1,1,2,2,2],[2,1,2,2,1,2,2,1,2],[2,2,1,2,1,2,1,2,2],[1,2,2,2,1,2,2,2,1],[4,4,4,4,1,4,4,4,4],[2,2,2,2,1,2,2,2,2],[4,4,4,4,1,4,4,4,4]]
        orbitLengths = [[(+1) $ length $ takeWhile (/= slot) $ tail $ iterate symmetry slot | slot <- slots] | symmetry <- symmetries]

        choiceFunction :: [[Int]] -> [[Int]]
        choiceFunction os = map removeSharedOrbits os

        removeSharedOrbits :: [Int] -> [Int]
        removeSharedOrbits os = removeSharedOrbits' (sort os)
            where
                removeSharedOrbits' os =
                    case os of
                        []   -> []
                        o:os -> o : removeSharedOrbits' (drop (o - 1) os)

        genExpression :: (Integral a, Ord b) => [b] -> [[a]] -> Polynomial Integer b
        genExpression cs orbitGroups = addPoly $ groupAndSort valuesForAllOrbits compare orbitGroups
            where
                -- Expression to choose one element out of cs for 'orbitLength' slots simultaneously (i.e. they are all the same).
                selectOneValue orbitLength cs = addPoly [expPoly (makeVar c) (toInteger orbitLength) | c <- cs]

                -- Expression to choose values for groups of slots with the same choice expression.
                selectIndependentValues slotGroupCount choiceExpression = expPoly choiceExpression (toInteger slotGroupCount)

                -- Choose independent values for a group of slots with the same orbit length.
                multipleIndependentValues orbitLength grp = [selectIndependentValues (length grp + 1) (selectOneValue orbitLength cs)]

                -- Choose independent values for all orbits of one symmetry operation,
                -- then multiply by the number of symmetries which share the same set of orbit lengths.
                valuesForAllOrbits orbits orbitGroup = [multPoly (makeConst (toInteger (1 + length orbitGroup)) : (groupAndSort multipleIndependentValues compare orbits))]
