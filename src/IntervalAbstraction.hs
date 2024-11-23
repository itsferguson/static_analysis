module IntervalAbstraction where

import Abstraction
import Language
import Memory

data Bound = NegInf | PosInf | Int Integer
  deriving (Eq)

instance Show Bound where
  show NegInf = "-∞"
  show PosInf = "+∞"
  show (Int n) = show n

instance Ord Bound where
  compare NegInf NegInf = EQ
  compare NegInf _ = LT
  compare _ NegInf = GT
  compare PosInf PosInf = EQ
  compare PosInf _ = GT
  compare _ PosInf = LT
  compare (Int a) (Int b) = compare a b

instance Num Bound where
  NegInf + (Int _) = NegInf
  (Int _) + NegInf = NegInf
  PosInf + (Int _) = PosInf
  (Int _) + PosInf = PosInf
  Int a + Int b = Int (a + b)

  abs NegInf = PosInf
  abs PosInf = PosInf
  abs (Int a) = Int (abs a)

  negate NegInf = PosInf
  negate PosInf = NegInf
  negate (Int a) = Int (negate a)

  NegInf * (Int a) = if signum a == 1 then NegInf else negate NegInf
  (Int a) * NegInf = NegInf * (Int a)
  PosInf * (Int a) = if signum a == 1 then PosInf else negate PosInf
  (Int a) * PosInf = PosInf * (Int a)
  Int a * Int b = Int (a * b)

  signum NegInf = Int (-1)
  signum PosInf = Int 1
  signum (Int a) = Int (signum a)

  fromInteger n = Int n

data IntervalAbstraction = Ibot | Interval Bound Bound
  deriving (Eq)

instance Num IntervalAbstraction where
  Ibot + _ = Ibot
  _ + Ibot = Ibot
  Interval a1 b1 + Interval a2 b2 = Interval (a1 + a2) (b1 + b2)

  Ibot * _ = Ibot
  _ * Ibot = Ibot
  Interval a1 b1 * Interval a2 b2 =
    Interval
      (minimum [a1 * a2, a1 * b2, b1 * a2, b1 * b2])
      (maximum [a1 * a2, a1 * b2, b1 * a2, b1 * b2])

  fromInteger n = Interval (Int n) (Int n)

  negate Ibot = Ibot
  negate (Interval a b) = Interval (negate b) (negate a)

  abs Ibot = Ibot
  abs (Interval a b) = createInterval (abs a) (abs b)

  signum Ibot = Ibot
  signum (Interval a b) = Interval (signum a) (signum b)

instance Show IntervalAbstraction where
  show Ibot = "⊥"
  show (Interval a b) = "[" ++ show a ++ ", " ++ show b ++ "]"

createInterval :: Bound -> Bound -> IntervalAbstraction
createInterval a b = if a < b then Interval a b else Interval b a

instance Abstraction IntervalAbstraction where
  bot = Ibot
  top = Interval NegInf PosInf

  filterMemory (Cinfeq, v, n) mem =
    case getVariable v mem of
      Ibot -> botMemory mem
      Interval a b
        | a > (Int n) -> botMemory mem
        | b < (Int n) -> mem
        | otherwise -> setVariable v (Interval a (Int n)) mem
  filterMemory (Csup, v, n) mem =
    case getVariable v mem of
      Ibot -> botMemory mem
      Interval a b
        | b <= (Int n) -> botMemory mem
        | a > (Int n) -> mem
        | otherwise -> setVariable v (Interval (Int (n + 1)) b) mem

  unionValue Ibot a = a
  unionValue a Ibot = a
  unionValue (Interval a1 b1) (Interval a2 b2) = Interval (min a1 a2) (max b1 b2)

  widenValue Ibot a = a
  widenValue a Ibot = a
  widenValue (Interval a1 b1) (Interval a2 b2)
    | a1 <= a2 && b1 >= b2 = Interval a1 b1
    | a1 <= a2 = Interval a1 PosInf
    | b1 >= b2 = Interval NegInf b1
    | otherwise = Interval NegInf PosInf
