module Memory where

import Data.Map as Map
import Language

data Memory a = Memory (Map.Map Var a)
  deriving (Eq)

instance (Show a) => Show (Memory a) where
  show (Memory m) = "Memory " ++ show (toList m)

initMemory :: Memory a
initMemory = Memory Map.empty

singletonMemory :: Var -> a -> Memory a
singletonMemory v x = Memory (Map.singleton v x)

isElement :: (Eq a) => a -> Memory a -> Bool
isElement x (Memory mem) = elem x (elems mem)

getVariable :: Var -> Memory a -> a
getVariable var (Memory mem) = case Map.lookup var mem of
  Just v -> v
  Nothing -> error ("Variable " ++ show var ++ " not found in memory")

setVariable :: Var -> a -> Memory a -> Memory a
setVariable v x (Memory mem) = Memory (Map.insert v x mem)

unionWith :: (a -> a -> a) -> Memory a -> Memory a -> Memory a
unionWith f (Memory mem1) (Memory mem2) = Memory (Map.unionWith f mem1 mem2)

map :: (a -> b) -> Memory a -> Memory b
map f (Memory mem) = Memory (Map.map f mem)
