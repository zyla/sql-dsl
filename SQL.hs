{-# LANGUAGE ConstraintKinds, GADTs, TypeFamilies, ScopedTypeVariables, FlexibleInstances, DataKinds, MultiParamTypeClasses, TypeOperators #-}

import GHC.TypeLits
import Data.Proxy

data Binding (name :: Symbol) a

type family VarType name scope :: * where
    VarType name (Binding name a ': scope) = a
    VarType name (b ': scope) = VarType name scope

type family HasVar' name scope :: Bool where
    HasVar' name (Binding name a ': scope) = True
    HasVar' name (b ': scope) = HasVar' name scope
    HasVar' name s = False

type HasVar name scope = HasVar' name scope ~ True

data Expr (scope :: [*]) a where
    Const :: a -> Expr scope a
    Var :: (KnownSymbol name, HasVar name scope)
        => Proxy name
        -> Expr scope (VarType name scope)
    Let :: KnownSymbol name
        => Proxy name -- ^ variable name
        -> Expr scope a -- ^ variable value
        -> Expr (Binding name a ': scope) b -- ^ body of let-block
        -> Expr scope b
