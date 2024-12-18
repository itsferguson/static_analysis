module Abstraction where

import Data.IntMap (member)
import Language
import Memory

num_unions = 1

class (Eq a, Num a) => Abstraction a where
  analyzeExpr :: Expr -> Memory a -> a
  analyzeCommand :: Command -> Memory a -> Memory a
  filterMemory :: Condition -> Memory a -> Memory a
  unionValue :: a -> a -> a
  union :: Memory a -> Memory a -> Memory a
  widenValue :: a -> a -> a
  widen :: Memory a -> Memory a -> Memory a
  analyze :: Command -> Memory a
  analyze cmd = analyzeCommand cmd initMemory
  botMemory :: Memory a
  botMemory = Memory.singletonMemory 'b' bot
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

  analyzeCommand _ mem
    | Memory.isElement bot mem = botMemory
  analyzeCommand (Cassign v e) mem = setVariable v (analyzeExpr e mem) mem
  analyzeCommand Cskip mem = mem
  analyzeCommand (Cseq c1 c2) mem = analyzeCommand c2 (analyzeCommand c1 mem)
  analyzeCommand (Cinput v) mem = setVariable v top mem
  analyzeCommand (Cif b c1 c2) mem =
    let not_b = negateCondition b
        mem1 = analyzeCommand c1 (filterMemory b mem)
        mem2 = analyzeCommand c2 (filterMemory not_b mem)
     in union mem1 mem2
  analyzeCommand (Cwhile cond c) mem = filterMemory (negateCondition cond) (unroll_loop num_unions mem)
    where
      loop_body mem = analyzeCommand c (filterMemory cond mem)
      unroll_loop 0 mem = (lfp loop_body mem)
      unroll_loop n mem = union mem (unroll_loop (n - 1) (loop_body mem))
      lfp f mem = if r == mem then r else lfp f r
        where
          r = widen mem (f mem)
  union mem1 mem2
    | Memory.isElement bot mem1 = mem2
    | Memory.isElement bot mem2 = mem1
    | otherwise = Memory.unionWith unionValue mem1 mem2
  widen mem1 mem2
    | Memory.isElement bot mem1 = mem2
    | Memory.isElement bot mem2 = mem1
    | otherwise = Memory.unionWith widenValue mem1 mem2
