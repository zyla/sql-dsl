{-# LANGUAGE UndecidableInstances, PolyKinds, ConstraintKinds, GADTs, TypeFamilies, ScopedTypeVariables, FlexibleInstances, DataKinds, MultiParamTypeClasses, TypeOperators #-}

import Text.Show
import GHC.TypeLits

data Binding (name :: Symbol) a
data Table (name :: Symbol) (columns :: [*])
data BoundTable (name :: Symbol) (columns :: [*])

data Found (a :: k)
data VarNotFound name
data TableNotFound name

type family VarType name scope :: * where
    VarType name (Binding name a ': scope) = Found a
    VarType name (Table tableName columns ': scope) = VarType name columns ||. VarType name scope
    VarType name (b ': scope) = VarType name scope
    VarType name s = VarNotFound name

type Var name a scope = VarType name scope ~ Found a

type family TableColumns (table :: Symbol) (scope :: [*]) :: * where
    TableColumns name (Table name columns ': scope) = Found columns
    TableColumns name (b ': scope) = TableColumns name scope
    TableColumns name s = TableNotFound name

type IsTable table columns scope = TableColumns table scope ~ Found columns

type family BoundTableColumns (table :: Symbol) (scope :: [*]) :: * where
    BoundTableColumns name (BoundTable name columns ': scope) = Found columns
    BoundTableColumns name (b ': scope) = BoundTableColumns name scope
    BoundTableColumns name s = TableNotFound name

type IsBoundTable table columns scope = BoundTableColumns table scope ~ Found columns

type family (++) (a :: [*]) (b :: [*]) where
    '[] ++ xs = xs
    (x ': xs) ++ ys = x ': (xs ++ ys)

type family (||.) a b where
    Found a ||. x = Found a
    notFound ||. x = x

data Operator a b c where
    Eq :: Operator a a Bool
    Add :: Operator Int Int Int
    -- TODO

opPrec :: Operator a b c -> Int
opPrec Eq = 1
opPrec Add = 2

opString :: Operator a b c -> String
opString Eq = "="
opString Add = "+"

data Expr (scope :: [*]) a where
    -- A constant
    Const :: Show a -- FIXME
          => a
          -> Expr scope a

    -- Variable selector. Requires the variable to be in scope
    Var :: Var name a scope
        => Sym name
        -> Expr scope a

    -- Column selector "table.col". Requires table to be in scope and have given column.
    Col :: ( IsBoundTable table columns scope
           , Var col a columns
           )
        => Sym table
        -> Sym col
        -> Expr scope a

    -- Operator. Reuires types to match.
    Op :: Operator a b c
       -> Expr scope a
       -> Expr scope b
       -> Expr scope c

    -- Subquery. Requires the query to have only one column.
    SubSelect :: Select scope '[Binding whatever a] -> Expr scope (Maybe a)

instance Show (Expr scope a) where
    showsPrec p (Const x) = showsPrec p x
    showsPrec _ (Var name) = showSymbol name
    showsPrec _ (Col table col) = showSymbol table . showChar '.' . showSymbol col
    showsPrec p (Op operator a b) =
        let prec = opPrec operator
        in showParen (p > prec) $
            showsPrec prec a . showChar ' ' .
            showString (opString operator) . showChar ' ' .
            showsPrec prec b

data Columns (scope :: [*]) (bindings :: [*]) where
    ColNil :: Columns scope '[]
    ColCons :: Sym name
            -> Expr scope a
            -> Columns scope bindings
            -> Columns scope (Binding name a ': bindings)

instance Show (Columns scope (b ': bs)) where
    showsPrec p (ColCons name expr ColNil) = showsPrec p expr . showString " AS " . showSymbol name
    showsPrec p (ColCons name expr xs@ColCons{}) =
        showsPrec p expr . showString " AS " . showSymbol name .
        showString ", " . showsPrec p xs

showSymbol :: Sym sym -> ShowS
showSymbol sym@Sym = showString $ symbolVal sym

data Sym (val :: Symbol) where
    Sym :: KnownSymbol val => Sym val

data OrderBy scope = forall a. Asc (Expr scope a) | forall a. Desc (Expr scope a)

instance Show (OrderBy scope) where
    showsPrec p (Asc expr) = showsPrec p expr . showString " ASC"
    showsPrec p (Desc expr) = showsPrec p expr . showString " DESC"

data Select (scope :: [*]) (a :: [*]) where
    Select :: ( IsTable table tableColumns scope
              , selectScope ~ (BoundTable tableAlias tableColumns ': (tableColumns ++ scope))
              , orderScope ~ (bindings ++ selectScope)
              , bindings ~ (atLeastOneBinding ': xs)
              )
           => Columns selectScope bindings
           -> Sym table -- ^ FROM
           -> Maybe (Sym tableAlias) -- ^ alias
           -> Maybe (Expr selectScope Bool) -- ^ WHERE
           -> [OrderBy orderScope]
           -> Select scope bindings

instance Show (Select scope bindings) where
    showsPrec p' (Select columns table alias cond order) = showParen (p' > 2) $
        let p = 3 in
        showString "SELECT " .
        showsPrec p columns . 
        showString " FROM " .
        showSymbol table .
        maybe id ((showChar ' ' .) . showSymbol) alias .
        maybe id ((showString " WHERE " .) . showsPrec p) cond .
        (case order of
            [] -> id
            (_:_) -> showString " ORDER BY " .
                foldr1 (\a b -> a . showString ", " . b) (map (showsPrec p) order)
        )

type AppScope = '[UsersTable]
type UsersTable = Table "users"
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
    (ColCons (Sym :: Sym "user_id") (Var (Sym :: Sym "id")) $
     ColCons (Sym :: Sym "username")
        (Col (Sym :: Sym "fancy_alias_for_users") (Sym :: Sym "name")) $
    ColNil)
    {-FROM-} (Sym :: Sym "users")
    (Just {-AS-} (Sym :: Sym "fancy_alias_for_users"))
    (Just {-WHERE-} (Var (Sym :: Sym "admin")))
    [Asc (Var (Sym :: Sym "username"))]
