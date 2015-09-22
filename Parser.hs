{-# LANGUAGE FlexibleContexts #-}
module Parser where

import AST
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec.Number
import Data.Char

expr :: Parser Expr
expr = buildExpressionParser table term

parens expr = lexeme (char '(') *> expr <* lexeme (char ')')

term = parens expr <|> litNumber <|> litString <|> var

lexeme p = p <* many (satisfy isSpace)

litNumber = lexeme $ LitNumber <$> decimal

litString = fmap LitString $
    char '\'' *> many strChar <* char '\''
  where
    strChar = escape <|> satisfy (/= '\'')
    escape = do
        char '\\'
        unescape <$> anyChar
    unescape 'n' = '\n' -- TODO
    unescape x = x

var = do
    first <- identifier
    try (Col first <$> (lexeme (char '.') >> identifier)) <|> return (Var first)

identifier = lexeme $ many1 $ satisfy isAlpha

table   = [ [binary "*" Mul, binary "/" Div ]
          , [binary "+" Add, binary "-" Sub ]
          ]
  where
    binary str op = Infix (lexeme (string str) >> return (Op op)) AssocLeft
