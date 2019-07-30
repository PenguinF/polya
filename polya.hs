module Eutherion.Polya (

       PolyaGroup

       ) where

-- Encapsulates a list of elements, and a list of symmetry operations acting on those elements.
data PolyaGroup a = PolyaGroup [a] [a -> a]
