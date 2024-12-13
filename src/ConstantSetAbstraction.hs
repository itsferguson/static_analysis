module ConstantSetAbstraction where

import Abstraction
import Data.Set as Set
import Language
import Memory

data SetAbstraction = SBot | SetAbstraction (Set Integer) | STop
  deriving (Eq)

max_size = 100

applyFunction :: (Ord a) => (a -> a -> a) -> (Set a) -> (Set a) -> (Set a)
applyFunction f s1 s2 = Set.map (\(a, b) -> f a b) (Set.cartesianProduct s1 s2)

instance Show SetAbstraction where
  show (SetAbstraction s) = "{" ++ showValues (toList s) ++ "}"
    where
      showValues (a : []) = show a
      showValues (a : as) = show a ++ ", " ++ showValues as
  show SBot = "Bot"
  show STop = "Top"

instance Num SetAbstraction where
  (SetAbstraction s1) + (SetAbstraction s2) = SetAbstraction (applyFunction (+) s1 s2)
  _ + _ = STop

  (SetAbstraction s1) * (SetAbstraction s2) = SetAbstraction (applyFunction (*) s1 s2)
  _ * _ = STop

  (SetAbstraction s1) - (SetAbstraction s2) = SetAbstraction (applyFunction (-) s1 s2)
  _ - _ = STop

  fromInteger n = SetAbstraction (singleton (fromInteger n))

  abs (SetAbstraction s1) = SetAbstraction (fromList [abs x | x <- toList s1])
  signum (SetAbstraction s1) = SetAbstraction (fromList [signum x | x <- toList s1])

instance Abstraction SetAbstraction where
  bot = SetAbstraction empty
  top = STop

  filterMemory (comp, v, n) mem = case x of
    SBot -> botMemory
    SetAbstraction s -> setVariable v (SetAbstraction (filteredSet s)) mem
    STop -> mem
    where
      x = getVariable v mem
      rel Rleq = (<=)
      rel Rgeq = (>=)
      rel Rgt = (>)
      rel Rle = (<)
      filteredSet s = Set.filter (\elem -> (rel comp) elem n) s

  unionValue SBot _ = SBot
  unionValue _ SBot = SBot
  unionValue (SetAbstraction s1) (SetAbstraction s2) = SetAbstraction (Set.union s1 s2)
  unionValue _ _ = STop

  widenValue SBot _ = SBot
  widenValue _ SBot = SBot
  widenValue s1 s2 = case unionValue s1 s2 of
    SetAbstraction s -> if Set.size s > max_size then STop else SetAbstraction s
    a -> a
