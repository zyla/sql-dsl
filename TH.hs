{-# LANGUAGE TemplateHaskell #-}
module TH where

import Data.Traversable

import AST
import SQL
import Parser

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

liftSelect :: AST.Select -> Q Exp
liftSelect AST.Select { selectTargetList = targetList
                      , selectFromClause = fromClause
                      , selectWhereClause = maybeWhereClause
                      , selectSortClause = sortClause
                      } =
    [|SQL.Select $(mkColumns targetList)
                 $(liftFromClause fromClause)
                 $(liftMaybe $ fmap liftExpr maybeWhereClause)
                 $(ListE <$> traverse ordToTH sortClause)
    |]


liftFromClause :: [TableRef] -> Q Exp
liftFromClause [] = [|FromNil|]
liftFromClause (TableRef table Nothing:xs) = [|FromTable $(mkSym table) $(liftFromClause xs)|]
liftFromClause (TableRef table (Just alias):xs) = [|FromTableAlias $(mkSym table) $(mkSym alias) $(liftFromClause xs)|]
liftFromClause (SelectRef select alias:xs) = [|FromSubSelect $(mkSym alias) $(liftSelect select) $(liftFromClause xs)|]

-- | Lift Maybe constructor surrounding the TH splice to TH level.
-- 'lift' won't work, because it has to recursively lift whole data structure.
liftMaybe :: Maybe (Q Exp) -> Q Exp
liftMaybe Nothing = [|Nothing|]
liftMaybe (Just x) = [|Just $(x)|]

mkColumns :: [ResultTarget] -> Q Exp
mkColumns [] = [|SQL.ColNil|]
mkColumns (ExprTarget expr (Just alias):xs) =
    [|SQL.ColCons $(mkSym alias) $(liftExpr expr) $(mkColumns xs)|]

liftExpr :: AST.Expr -> Q Exp
liftExpr (LitNumber x) = [|SQL.Const $(lift x)|]
liftExpr (AST.Var x) = [|SQL.Var $(mkSym x)|]

ordToTH (SortBy AST.Asc expr) = [|SQL.Asc $(liftExpr expr)|]
ordToTH (SortBy AST.Desc expr) = [|SQL.Desc $(liftExpr expr)|]

mkSym :: String -> Q Exp
mkSym str = [|SQL.Sym :: SQL.Sym $(return $ LitT $ StrTyLit str)|]

sql :: QuasiQuoter
sql = QuasiQuoter
    { quoteExp = \input ->
        case parseSelect input of
            Left err -> error $ show err
            Right select -> liftSelect select
    , quotePat = undefined -- TODO
    , quoteType = undefined
    , quoteDec = undefined
    }
