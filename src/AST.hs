module AST where

data Expr
    = Var String
    | Bool Bool
    | Tuple [Expr]
    | TupleAccess Expr Expr
    | Let String Expr Expr
    | If Expr Expr Expr
    | Lambda String Expr
    deriving(Eq, Ord, Show)
