module Parser where

import Control.Applicative
import Data.Char (isDigit, isLetter)

-- A parser is a function that extracts a value from the prefix of a
-- string, resulting in the extracted value and the unused sufix of
-- the string, with the possibility of failure.
newtype Parser a = MkP (String -> Maybe (a, String))

-- Apply a parser to a string
parse :: Parser a -> String -> Maybe (a, String)
parse (MkP fun) = fun

-- Parser to extract the next char of the string
item :: Parser Char
item = MkP ( \s -> case s of
                     x:xs -> Just (x, xs)
                     "" -> Nothing
           )

-- Constructs a new parser by applying a given parser and verifying
-- that the result value satisfies a given property
satisfy :: (a -> Bool) -> Parser a -> Parser a
satisfy test p =
  MkP ( \s -> case parse p s of
                Just (x, s') | test x -> Just (x, s')
                _ -> Nothing
      )

-- Parser for a given char
char :: Char -> Parser Char
char x = satisfy (== x) item

-- Parsers a functors
instance Functor Parser where
  -- Apply a function to the result of parser
  fmap f p = MkP ( \s -> case parse p s of
                           Just (x, s') -> Just (f x, s')
                           Nothing -> Nothing
                 )

-- Parsers are applicative functors
instance Applicative Parser where
  -- A parser that returns the give value
  pure x = MkP ( \s -> Just (x, s) )

  -- Parser sequencing: the first parser gives a function, the second
  -- parser gives a value (using what remained from the string given
  -- to the first parser), the resulting parser applies the function
  -- to the value
  pf <*> px = MkP ( \s -> case parse pf s of
                            Just (f, s') -> parse (f <$> px) s'
                            Nothing -> Nothing
                  )

-- Parsers are alternative functors
instance Alternative Parser where
  -- A parser that always fails
  empty = MkP ( \_ -> Nothing )

  -- Parser alternatives: if the first parse fails, the second parser
  -- is used instead
  p1 <|> p2 = MkP ( \s -> case parse p1 s of
                            Nothing -> parse p2 s
                            Just (x, s') -> Just (x, s')
                  )

-- Parsers are monads
instance Monad Parser where
  -- A parser that returns the given value
  return x = MkP ( \s -> Just (x, s) )

  -- Monadic parser sequencing: the second argument is a function that
  -- is applied to the result of the first parser using its remaining
  -- string
  p >>= f = MkP ( \s -> case parse p s of
                          Nothing -> Nothing
                          Just (x, s') -> parse (f x) s'
                )

-- Chaining parser from left to right. It accepts the empty string.
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op x = chainl1 p op <|> return x

-- Chaining parser from left to right.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = do f <- op
                y <- p
                rest (f x y)
             <|>
             return x

-- Chaining parser from right to left. It accepts the empty string.
chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op x = chainr1 p op <|> return x

-- Chaining parser from right to left. It accepts the empty string.
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
  where
    scan = p >>= rest
    rest x = do f <- op
                y <- scan
                return (f x y)
             <|>
             return x

-- A parser for a digit
digit :: Parser Char
digit = satisfy isDigit item

-- A parser for a letter
letter :: Parser Char
letter = satisfy isLetter item

-- A parser for an underscore
underscore :: Parser Char
underscore = char '_'

-- A parser for natural numbers
integer :: Parser Integer
integer = read <$> some digit

-- A parser for identifiers
identifier :: Parser String
identifier = (:) <$> letter <*> many (letter <|> digit <|> underscore)

-- A parser for white spaces
spaces :: Parser String
spaces = many (char ' ' <|> char '\t' <|> char '\n')

-- A parser that applies a given parser to the input and extracts
-- white spaces following it
token :: Parser a -> Parser a
token p = p <* spaces

-- A parser for a given string
string :: String -> Parser String
string "" = pure ""
string (x:xs) = (:) <$> char x <*> string xs
