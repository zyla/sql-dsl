{-# LANGUAGE UndecidableInstances, ConstraintKinds, GADTs, TypeFamilies, ScopedTypeVariables, FlexibleInstances, DataKinds, MultiParamTypeClasses, TypeOperators #-}

import GHC.TypeLits
import Data.Proxy

data Binding (name :: Symbol) a
data Table' (name :: Symbol) (columns :: [*])

type family VarType name scope :: * where
    VarType name (Binding name a ': scope) = a
    VarType name (b ': scope) = VarType name scope

type family HasVar' name scope :: Bool where
    HasVar' name (Binding name a ': scope) = True
    HasVar' name (Table' tableName columns ': scope) = HasVar' name columns || HasVar' name scope
    HasVar' name (b ': scope) = HasVar' name scope
    HasVar' name s = False

type HasVar name scope = HasVar' name scope ~ True

type family TableColumns (table :: Symbol) (scope :: [*]) :: [*] where
    TableColumns name (Table' name columns ': scope) = columns
    TableColumns name (b ': scope) = TableColumns name scope

type Table table columns scope = (HasTable table scope, TableColumns table scope ~ columns)

type family HasTable' name scope :: Bool where
    HasTable' name (Table' name a ': scope) = True
    HasTable' name (b ': scope) = HasTable' name scope
    HasTable' name s = False

type HasTable name scope = HasTable' name scope ~ True

type family (++) (a :: [*]) (b :: [*]) where
    '[] ++ xs = xs
    (x ': xs) ++ ys = x ': (xs ++ ys)

type family (||) (a :: Bool) (b :: Bool) where
    True || x = True
    False || x = x

data Operator a b c where
    Eq :: Operator a a Bool
    Add :: Operator Int Int Int
    -- TODO

data Expr (scope :: [*]) a where
    -- A constant
    Const :: a -> Expr scope a

    -- Variable selector. Requires the variable to be in scope
    Var :: (KnownSymbol name, HasVar name scope)
        => Proxy name
        -> Expr scope (VarType name scope)

    -- Column selector "table.col". Requires table to be in scope and have given column.
    Col :: ( KnownSymbol table, KnownSymbol col
           , Table table columns scope
           , HasVar col columns
           )
        => Proxy table
        -> Proxy col
        -> Expr scope (VarType col columns)

    -- Operator. Reuires types to match.
    Op :: Operator a b c
       -> Expr scope a
       -> Expr scope b
       -> Expr scope c

    -- Subquery. Requires the query to have only one column.
    SubSelect :: Select scope '[Binding whatever a] -> Expr scope (Maybe a)

data Columns (scope :: [*]) (bindings :: [*]) where
    ColNil :: Columns scope '[]
    ColCons :: KnownSymbol name
            => Proxy name
            -> Expr scope a
            -> Columns scope bindings
            -> Columns scope (Binding name a ': bindings)

data OrderBy scope = forall a. Asc (Expr scope a) | forall a. Desc (Expr scope a)

data Select (scope :: [*]) (a :: [*]) where
    Select :: ( Table table tableColumns scope
              , selectScope ~ (Table' tableAlias tableColumns ': (tableColumns ++ scope))
              , orderScope ~ (bindings ++ selectScope)
              )
           => Columns selectScope bindings
           -> Proxy table -- ^ FROM
           -> Maybe (Proxy tableAlias) -- ^ alias
           -> Maybe (Expr selectScope Bool) -- ^ WHERE
           -> [OrderBy orderScope]
           -> Select scope bindings

type AppScope = '[UsersTable]
type UsersTable = Table' "users"
    '[ Binding "id" Int
     , Binding "name" String
     , Binding "admin" Bool
     ]

-- SELECT id AS user_id, fancy_alias_for_users.name AS username
-- FROM users AS fancy_alias_for_users
-- WHERE admin
-- ORDER BY username DESC
example :: Select AppScope '[Binding "user_id" Int, Binding "username" String]
example = Select
    (ColCons (Proxy :: Proxy "user_id") (Var (Proxy :: Proxy "id")) $
     ColCons (Proxy :: Proxy "username")
        (Col (Proxy :: Proxy "fancy_alias_for_users") (Proxy :: Proxy "name")) $
    ColNil)
    {-FROM-} (Proxy :: Proxy "users")
    (Just {-AS-} (Proxy :: Proxy "fancy_alias_for_users"))
    (Just {-WHERE-} (Var (Proxy :: Proxy "admin")))
    [Asc (Var (Proxy :: Proxy "username"))]
