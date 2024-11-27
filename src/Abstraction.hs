module Abstraction where

import Data.IntMap (member)
import Language
import Memory

class (Eq a, Num a) => Abstraction a where
  analyzeExpr :: Expr -> Memory a -> a
  analyzeCommand :: Command -> Memory a -> Memory a
  filterMemory :: Condition -> Memory a -> Memory a
  unionValue :: a -> a -> a
  union :: Memory a -> Memory a -> Memory a
  widenValue :: a -> a -> a
  widen :: Memory a -> Memory a -> Memory a
  memoryIsBot :: Memory a -> Bool
  analyze :: Command -> Memory a
  analyze cmd = analyzeCommand cmd initMemory
  botMemory :: Memory a -> Memory a
  botMemory = Memory.map (const bot)
  bot :: a
  top :: a

  analyzeExpr (Econst n) mem = fromInteger n
  analyzeExpr (Evar v) mem = getVariable v mem
  analyzeExpr (Ebop op e1 e2) mem =
    let v1 = analyzeExpr e1 mem
        v2 = analyzeExpr e2 mem
     in case op of
          Badd -> v1 + v2
          Bsub -> v1 + negate v2
          Bmul -> v1 * v2

  memoryIsBot = elem bot

  analyzeCommand _ mem
    | memoryIsBot mem = mem
  analyzeCommand (Cassign v e) mem = setVariable v (analyzeExpr e mem) mem
  analyzeCommand Cskip mem = mem
  analyzeCommand (Cseq c1 c2) mem = analyzeCommand c2 (analyzeCommand c1 mem)
  analyzeCommand (Cinput v) mem = setVariable v top mem
  analyzeCommand (Cif b c1 c2) mem =
    let not_b = negateCondition b
        mem1 = analyzeCommand c1 (filterMemory b mem)
        mem2 = analyzeCommand c2 (filterMemory not_b mem)
     in union mem1 mem2
  analyzeCommand (Cwhile cond c) mem = filterMemory (negateCondition cond) (lfp loop_body mem)
    where
      loop_body mem = analyzeCommand c (filterMemory cond mem)
      lfp f mem =
        let r = widen mem (f mem)
         in if r == mem then r else lfp f r
  union = Memory.unionWith unionValue
  widen = Memory.unionWith widenValue
