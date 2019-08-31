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
import Eutherion.Polynomial

-- Encapsulates a list of elements, and a list of symmetry operations acting on those elements.
data PolyaGroup a = PolyaGroup [a] [a -> a]

makePolyaGroup :: [a] -> [a -> a] -> PolyaGroup a
makePolyaGroup slots fns = PolyaGroup slots fns

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
characteristic :: (Eq a, Ord b) => PolyaGroup a -> [b] -> (Integer, Polynomial Integer b)
characteristic (PolyaGroup slots symmetries) cs =
    (toInteger $ length symmetries, genExpression cs $ choiceFunction orbitLengths)
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
                singleChoice cs = addPoly [makeVar c | c <- cs]
                multipleChoice orbitLength cs = addPoly [expPoly (makeVar c) (toInteger orbitLength) | c <- cs]
                selectOneValue orbitLength = case orbitLength of
                    1           -> singleChoice
                    orbitLength -> multipleChoice orbitLength

                -- Expression to choose values for groups of slots with the same choice expression.
                selectIndependentValues slotGroupCount choiceExpression = case slotGroupCount of
                    1              -> choiceExpression
                    slotGroupCount -> expPoly choiceExpression (toInteger slotGroupCount)

                -- Choose independent values for a group of slots with the same orbit length.
                multipleIndependentValues orbitLength grp = [selectIndependentValues (length grp + 1) (selectOneValue orbitLength cs)]

                -- Choose independent values for all orbits of one symmetry operation,
                -- then multiply by the number of symmetries which share the same set of orbit lengths.
                valuesForAllOrbits orbits orbitGroup = [multPoly (makeConst (toInteger (1 + length orbitGroup)) : (groupAndSort multipleIndependentValues compare orbits))]
