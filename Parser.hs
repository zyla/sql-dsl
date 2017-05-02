{-# LANGUAGE FlexibleContexts #-}
module Parser where

import AST
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec.Number
import Data.Char
import Data.Maybe
import Control.Monad

keywords = words $ "SELECT FROM WHERE ORDER ASC DESC AND OR"
anyKeyword = choice $ map sqlKeyword keywords

expr :: Parser Expr
expr = buildExpressionParser table term

parens expr = lexeme (char '(') *> expr <* lexeme (char ')')

term = parens expr <|> litNumber <|> litString <|> var

lexeme p = p <* many (satisfy isSpace)

sqlKeyword = lexeme . mapM (\c -> oneOf [toLower c, toUpper c])
sqlKeywords = mapM sqlKeyword . words

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

whitespace = many (satisfy isSpace)

identifier = lexeme $ do
  id <- many1 identifierChar <?> "identifier"
  when (map toUpper id `elem` keywords) $
    unexpected $ show id
  return id

identifierChar = satisfy isAlpha <|> char '_'

table   = [ [binary "*" Mul, binary "/" Div ]
          , [binary "+" Add, binary "-" Sub ]
          , [binary "=" Eq ]
          , [binary "AND" And ]
          , [binary "OR" Or ]
          ]
  where
    binary str op = Infix (try (lexeme (string str)) >> return (Op op)) AssocLeft

selectClause = sqlKeyword "SELECT" >>
    Select <$> sepBy1 (try resultTarget) comma
           <*> option [] fromClause
           <*> optionMaybe whereClause
           <*> option [] sortClause

resultTarget = (ExprTarget
    <$> expr
    <*> optionMaybe (try $ optional (sqlKeyword "AS") >> identifier)) <?> "result target"

fromClause = sqlKeyword "FROM" >> sepBy1 tableRef (try comma)

tableRef = do
    name <- identifier
    alias <- optionMaybe $ try $ optional (sqlKeyword "AS") >> identifier
    return $ TableRef name alias

whereClause = sqlKeyword "WHERE" >> expr

comma = lexeme $ char ','

sortClause = sqlKeywords "ORDER BY" >> sepBy1 sortBy comma
  where
    sortBy = do
        e <- expr
        t <- option Asc sortByType
        return $ SortBy t e
    sortByType = (sqlKeyword "ASC" >> return Asc) <|> (sqlKeyword "DESC" >> return Desc)

parseSelect = parse (whitespace *> selectClause <* eof) ""
