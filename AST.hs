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

-- Taken from: https://github.com/postgres/postgres/blob/master/src/include/nodes/parsenodes.h
-- The commented out entries are TODO

data Select = Select
    {
    -- selectDistinctClause
    -- selectIntoClause
      selectTargetList :: [ResultTarget]
    , selectFromClause :: [TableRef]
    , selectWhereClause :: Maybe Expr
    -- selectGroupClause
    -- selectHavingClause
    -- selectWindowClause
    , selectSortClause :: [SortBy]
    -- selectLimitOffset
    -- selectLimitCount
    -- selectLockingClause
    -- selectWithClause
    } deriving (Eq, Show)

data ResultTarget = AllTarget | ExprTarget Expr (Maybe String) deriving (Eq, Show)

data TableRef = TableRef String (Maybe String) | SelectRef Select String deriving (Eq, Show)

data SortBy = SortBy SortByType Expr deriving (Eq, Show)

data SortByType = Asc | Desc deriving (Eq, Show)
