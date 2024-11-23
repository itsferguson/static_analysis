module Memory where

import Data.Map as Map
import Language

type Memory a = Map.Map Var a

initMemory :: Memory a
initMemory = Map.empty

getVariable :: Var -> Memory a -> a
getVariable var mem = case Map.lookup var mem of
  Just v -> v
  Nothing -> error ("Variable " ++ show var ++ " not found in memory")

setVariable :: Var -> a -> Memory a -> Memory a
setVariable = Map.insert

unionWith :: (a -> a -> a) -> Memory a -> Memory a -> Memory a
unionWith = Map.unionWith

map :: (a -> b) -> Memory a -> Memory b
map = Map.map
