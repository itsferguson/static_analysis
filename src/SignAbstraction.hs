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

  filterMemory (Rleq, v, n) mem = case (x, compare n 0) of
    (Abot, _) -> botMemory
    (Azero, LT) -> botMemory
    (Azero, _) -> mem
    (Apos, LT) -> botMemory
    (Apos, EQ) -> setVariable v Azero mem
    (Apos, GT) -> mem
    (Atop, GT) -> mem
    (Atop, _) -> setVariable v Aneg mem
    (Aneg, _) -> mem
    where
      x = getVariable v mem
  filterMemory (Rgt, v, n) mem = case (x, compare n (-1)) of
    (Abot, _) -> botMemory
    (Azero, GT) -> botMemory
    (Azero, _) -> mem
    (Aneg, GT) -> botMemory
    (Aneg, EQ) -> setVariable v Azero mem
    (Aneg, LT) -> mem
    (Atop, LT) -> mem
    (Atop, _) -> setVariable v Apos mem
    (Apos, _) -> mem
    where
      x = getVariable v mem
  filterMemory (Rle, v, n) mem = filterMemory (Rleq, v, n - 1) mem
  filterMemory (Rgeq, v, n) mem = filterMemory (Rgt, v, n - 1) mem

  unionValue Abot a = a
  unionValue a Abot = a
  unionValue Azero a = a
  unionValue a Azero = a
  unionValue Apos Apos = Apos
  unionValue Aneg Aneg = Aneg
  unionValue _ _ = Atop

  widenValue = unionValue
