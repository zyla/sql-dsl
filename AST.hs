module AST where

data Operator =
    Add | Sub | Mul | Div
  | Eq | NotEq | Less | LessEq | Greater | GreaterEq
  | And | Or
  | Concat
  deriving (Eq, Show)

data Expr =
    LitNumber Integer
  | LitString String
  | Var String
  | Col String String
  | Op Operator Expr Expr
  deriving (Eq, Show)
