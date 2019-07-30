module Eutherion.Polya (

       PolyaGroup,
       makePolyaGroup,
       cayleyTable

       ) where

import Data.Array
import Data.List

import Eutherion.Utilities
import Eutherion.Combinatorics

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
        fst3 (x, y, z) = x
        snd3 (x, y, z) = y
        thd3 (x, y, z) = z

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
