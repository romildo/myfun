module Interpreter where

import Language

-- A type to represent the memory, that is, that association of
-- variable names with values
type Memory = [(String, Double)]

-- initial memory
initialMemory :: Memory
initialMemory = []

-- Expression evaluation
eval :: Memory -> Expr -> Double
eval _ (Cte x) = x
eval m (Var v) =
  case lookup v m of
    Just x -> x
    Nothing -> 0
eval m (Bin op x y) =
  case op of
    Add -> vx + vy
    Sub -> vx - vy
    Mul -> vx * vy
    Div -> vx / vy
  where
    vx = eval m x
    vy = eval m y

-- Command execution
execute :: Memory -> Cmd -> IO Memory
execute m (Assign v e) = return ((v, eval m e) : m)
execute m (Print e) = do print (eval m e)
                         return m
