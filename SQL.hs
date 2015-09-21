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

data Expr (scope :: [*]) a where
    Const :: a -> Expr scope a
    Var :: (KnownSymbol name, HasVar name scope)
        => Proxy name
        -> Expr scope (VarType name scope)
    Op :: Operator a b c
       -> Expr scope a
       -> Expr scope b
       -> Expr scope c
    SubSelect :: Select scope '[Binding whatever a] -> Expr scope (Maybe a)

data Columns (scope :: [*]) (bindings :: [*]) where
    ColNil :: Columns scope '[]
    ColCons :: KnownSymbol name
            => Proxy name
            -> Expr scope a
            -> Columns scope bindings
            -> Columns scope (Binding name a ': bindings)

data Select (scope :: [*]) (a :: [*]) where
    Select :: Table table tableColumns scope
           => Columns (tableColumns ++ scope) bindings
           -> Proxy table -- ^ FROM
           -> Maybe (Expr (tableColumns ++ scope) Bool) -- ^ WHERE
           -> Select scope bindings

type AppScope = '[UsersTable]
type UsersTable = Table' "users"
    '[ Binding "id" Int
     , Binding "name" String
     , Binding "admin" Bool
     ]

example :: Select AppScope '[Binding "user_id" Int, Binding "username" String]
example = Select
    (ColCons (Proxy :: Proxy "user_id") (Var (Proxy :: Proxy "id")) $
     ColCons (Proxy :: Proxy "username") (Var (Proxy :: Proxy "name")) $
    ColNil)
    {-FROM-} (Proxy :: Proxy "users")
    (Just {-WHERE-} (Var (Proxy :: Proxy "admin")))

