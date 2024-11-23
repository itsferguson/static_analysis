module SignAbstraction where

import Abstraction
import Language
import Memory

data SignAbstraction = Abot | Azero | Apos | Aneg | Atop
  deriving (Eq)

instance Show SignAbstraction where
  show Abot = "⊥"
  show Azero = "0"
  show Apos = "+"
  show Aneg = "-"
  show Atop = "±"

instance Num SignAbstraction where
  Abot + _ = Abot
  _ + Abot = Abot
  Azero + a = a
  a + Azero = a
  Apos + Apos = Apos
  Aneg + Aneg = Aneg
  _ + _ = Atop

  Abot * _ = Abot
  _ * Abot = Abot
  Azero * _ = Azero
  _ * Azero = Azero
  Apos * Apos = Apos
  Aneg * Aneg = Apos
  Apos * Aneg = Aneg
  Aneg * Apos = Aneg
  _ * _ = Atop

  negate Abot = Abot
  negate Azero = Azero
  negate Apos = Aneg
  negate Aneg = Apos
  negate Atop = Atop

  abs Aneg = Apos
  abs a = a

  signum a = a

  fromInteger n
    | n == 0 = Azero
    | n < 0 = Aneg
    | n > 0 = Apos

instance Abstraction SignAbstraction where
  bot = Abot
  top = Atop

  filterMemory (Cinfeq, v, n) mem = case (x, compare n 0) of
    (Abot, _) -> botMemory mem
    (Azero, LT) -> botMemory mem
    (Azero, _) -> mem
    (Apos, LT) -> botMemory mem
    (Apos, EQ) -> setVariable v Azero mem
    (Apos, GT) -> mem
    (Atop, GT) -> mem
    (Atop, _) -> setVariable v Aneg mem
    (Aneg, _) -> mem
    where
      x = getVariable v mem
  filterMemory (Cinfeq, v, n) mem = case (x, compare n 0) of
    (Abot, _) -> botMemory mem
    (Azero, LT) -> botMemory mem
    (Azero, _) -> mem
    (Apos, LT) -> botMemory mem
    (Apos, EQ) -> setVariable v Azero mem
    (Apos, GT) -> mem
    (Atop, GT) -> mem
    (Atop, _) -> setVariable v Aneg mem
    (Aneg, _) -> mem
    where
      x = getVariable v mem

  unionValue Abot a = a
  unionValue a Abot = a
  unionValue Azero a = a
  unionValue a Azero = a
  unionValue a b
    | a == b = a
    | otherwise = Atop

  widenValue = unionValue
