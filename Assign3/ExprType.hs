module ExprType where

import           Data.List

data Expr a = Add (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)
            | Const a
            | Var String
            | Cos (Expr a)
            | Sin (Expr a)
            | Ln (Expr a)
            | NExp (Expr a)
            | Neg (Expr a)
            | Exp (Expr a) (Expr a)
            | Var String
  deriving (Eq, Show)
{- following function has been taken from github user deleuuwj1, link to her profile can be found in the README-}
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Const _)    = []
getVars (Var ident)  = [ident]
getVars (Cos e1)     = getVars e1
getVars (Sin e1)     = getVars e1
getVars (NExp e1)    = getVars e1
getVars (Ln e1)      = getVars e1
getVars (Var s)      = [s]
