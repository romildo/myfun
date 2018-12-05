module Language where

-- The type of expressions
-- TODO: power (right associative)
-- TODO: relational operators (=, !=, >, >=, <, <=)
-- TODO: logical operators (&&, ||)
-- TODO: unnary operator for changing the sign (-)
-- TODO: unnary operator for logical negation (!)
data Expr = Cte Double
          | Var String
          | Bin Op Expr Expr
          deriving (Show)

data Op = Add | Sub | Mul | Div
  deriving (Show)

-- The type of commands
-- TODO: read command: read <var>
-- TODO: block (sequence) command: { <c1> ; <c2> ; ... ; <cn> } (n >= 0)
-- TODO: conditional command: if <expr> then <cmd> else <cmd>
-- TODO: repetion command: while <expr> do <cmd>
data Cmd = Assign String Expr
         | Print Expr
         deriving (Show)
