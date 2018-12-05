module MyFunParser where

import Control.Applicative (some, (<|>))
import Parser
import Language

-- A parser for double
double :: Parser Double
double =
  do x <- integer
     y <- decimalPart
     return (fromIntegral x + y)            
  where
    decimalPart =
      do char '.'
         rest <- some digit
         return (read rest / 10 ^ (length rest))
      <|>
      return 0

-- A parser for constant expressions
cte :: Parser Expr
cte = Cte <$> token double

-- A parser for variable expressions
var :: Parser Expr
var = Var <$> token identifier

-- A parser for atomic expressions, that is, the simplest expressions,
-- with the highest precedence
atomic :: Parser Expr
atomic = cte <|>
         var <|>
         token (char '(') *> expr <* token (char ')')

-- A parser for a binary operator
binop :: String -> (a -> a -> a) -> Parser (a -> a -> a)
binop name function = token (string name) *> pure function

-- A parser for multiplication and division
mul :: Parser Expr
mul = chainl1 atomic (binop "*" (Bin Mul) <|> binop "/" (Bin Div))

-- A parser for addition and subtraction
add :: Parser Expr
add = chainl1 mul (binop "+" (Bin Add) <|> binop "-" (Bin Sub))

-- A parser for expressions
expr :: Parser Expr
expr = add


-- A parser for assignment command
assign :: Parser Cmd
assign = Assign <$> token identifier <*
                    token (string ":=") <*>
                    expr
-- ... or alternatively
assign2 :: Parser Cmd
assign2 = do id <- token identifier
             token (string ":=")
             e <- expr
             return (Assign id e)

-- A parser for print command
printcmd :: Parser Cmd
printcmd = Print <$> (token (string "print") *> expr)

-- A parser for command
cmd :: Parser Cmd
cmd = assign <|> printcmd
