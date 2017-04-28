# sql-dsl
Yet another SQL DSL for Haskell.

State: highly unusable.

The main interface is a quasiquoter that parses SQL (defined in `TH.hs`). Example:

```haskell
query = [sql| SELECT foo + bar AS baz FROM table |]
```

This generates a query using the `Select` data type (`SQL.hs`).

```haskell
data Select (scope :: [*]) (a :: [*]) where
  -- ...
```

The type parameters are:
- *scope*: type-level representation of environment - names that can be referenced in a query.
   In a top-level query this will be your database schema. GHC will check that only tables defined here are referenced,
   and use the definitions to infer column types.
- *a*: result type - a type-level list of `Binding`s, representing columns returned by the query.
