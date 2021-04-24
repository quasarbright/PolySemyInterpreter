module Interpreter where

import Polysemy
    ( Member, Sem, reinterpret2H, makeSem, pureT, runT, raise, transform, subsume )
import Data.Map(Map)
import qualified Data.Map as Map
import Polysemy.Reader ( ask, local, Reader, runReader )
import Polysemy.Error(Error, throw, runError)
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Arrow ((>>>))

data Expr
    = Var String
    | Bool Bool
    | If Expr Expr Expr
    | Let String Expr Expr
    | Lambda String Expr
    | App Expr Expr
    deriving (Eq, Ord, Show)

data RuntimeError
    = UnboundVar String
    | TypeError String
    deriving(Eq, Ord, Show)

type Env = Map String Value

data Value
    = VBool Bool
    | VLambda Env String Expr
    deriving(Eq, Ord, Show)

data Interpreter m a where
    ThrowError :: RuntimeError -> Interpreter m a
    LookupVar :: String -> Interpreter m Value
    WithVar :: String -> Value -> m a -> Interpreter m a
    GetEnv :: Interpreter m Env
    WithEnv :: Env -> m a -> Interpreter m a

makeSem ''Interpreter

reinterpretInterpreter :: Sem (Interpreter ': r) a -> Sem (Reader Env ': Error RuntimeError ': r) a
reinterpretInterpreter = reinterpret2H $ \case
    ThrowError err -> throw err
    LookupVar x -> do
        env <- ask
        case Map.lookup x env of
            Nothing -> throw $ UnboundVar x
            Just v -> pureT v
    WithVar x v m -> do
        m' <- runT m <&> reinterpretInterpreter <&> subsume <&> subsume
        raise $ local (Map.insert x v) m'
    GetEnv -> do
        env <- ask
        pureT env
    WithEnv env m -> do
        m' <- runT m <&> reinterpretInterpreter <&> subsume <&> subsume
        raise $ local (const env) m'

runInterpreter :: Env -> Sem (Interpreter ': r) a -> Sem r (Either RuntimeError a)
runInterpreter env = 
    reinterpretInterpreter
    >>> runReader env
    >>> runError

evalExpr :: Member Interpreter r => Expr -> Sem r Value
evalExpr = \case
    Var x -> lookupVar x
    Bool b -> return $ VBool b
    If cnd thn els -> evalExpr cnd >>= \case
        VBool True -> evalExpr thn
        VBool False -> evalExpr els
        _ -> throwError (TypeError "if expects bool")
    Let x rhs body -> do
        vRhs <- evalExpr rhs
        withVar x vRhs (evalExpr body)
    Lambda x body -> do
        env <- getEnv
        return $ VLambda env x body
    App f x -> do
        evalExpr f >>= \case
            VLambda env argname body -> do
                vx <- evalExpr x
                evalExpr body & withEnv env & withVar argname vx
            _ -> throwError (TypeError "applied non-function")
