{--
Module:ExprDiff
Description:Contains a typeclass definition which defines some built-in functions specific to the Expr type

NOTE: this module has been taken in its enitrety from github user barskyn, link to her profile can be found in the README
--}


{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module ExprDiff where

import           ExprType

import qualified Data.Map.Strict as Map
{-This module defines a type class for the type definition Expr-}

class DiffExpr a where linearization :: Map.Map String a -> Expr a -> String -> String -> String -> String -> Expr a
  multDiff :: Expr a -> Expr a -> Expr a -> String -> String -> String -> Expr a
  antiDeriv :: Expr a -> Expr a  
  eval :: Map.Map String a -> Expr a -> a
  simplify :: Map.Map String a -> Expr a -> Expr a
  partDiff :: String -> Expr a -> Expr a

  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  val :: a -> Expr a
  val x = Const x
  var :: String -> Expr a
  var x = Var x
  exponent_Calculator:: Expr a -> Expr a -> Expr a
  exppnent_Calculator b x = Exp b x
  negative_Calculator :: Expr a -> Expr a
  negative_Calculator x = Neg x
  sin_Calculator :: Expr a -> Expr a 
  sin_Calculator x = Sin x
  cos_Calculator :: Expr a -> Expr a 
  cos_Calculator = Cos x
  ln_Calculator :: Expr a -> Expr a 
  ln_Calculator x = Ln x
  e_Calculator:: Expr a -> Expr a 
  e_Calculator x = NExp x

{-This is the function definition of eval-}
instance (Floating a, Eq a) => DiffExpr a where
  eval vrs (Const x) = x
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Neg e) = (-1) * (eval vrs e)
  eval vrs (Cos e) = cos (eval vrs e)
  eval vrs (Sin e) = sin (eval vrs e)
  eval vrs (Ln e) = log (eval vrs e)
  eval vrs (Exp b x) = (eval vrs b) ** (eval vrs x)
  partDiff s (Var x) | x == s = (Const 1)
                     | otherwise = (Const 0) 
  partDiff _ (Const _) = Const 0
  partDiff s (Add e1 e2) = Add (partDiff s e1) (partDiff s e2)
  partDiff s (Mult e1 e2) = Add (Mult (partDiff s e1) e2) (Mult e1 (partDiff s e2))
  partDiff s (Ln e) = Mult (Exp e (Const (-1))) (partDiff s e)
  partDiff s (Exp (Const a) (Var b)) = Mult ((Exp (Const a) (Var b))) (Ln (Const a))
  partDiff s (Exp b x) = Mult (Mult x (Exp b (Add x (Const (-1))))) (partDiff s b) 
  partDiff s (Sin x) = Mult (Cos x) (partDiff s x)
  partDiff s (Cos x) = Mult (Neg (Sin x)) (partDiff s x)
  partDiff s (Exp e) = Mult (NExp e) (partDiff s e)

  {-This function tries to simplify Expr type, REFERENCE:github user barskyn-}
  simplify vrs (Const a) = Const a
  simplify vrs (Neg e1) = Const (eval vrs (Neg e1))
  simplify vrs (Var s) = Const (eval vrs (Var s))
  simplify vrs (Exp e1 e2) = Const (eval vrs (Exp e1 e2))  
  simplify vrs (Sin e1) = Const (eval vrs (Sin e1))
  simplify vrs (Cos e1) = Const (eval vrs (Cos e1))
  simplify vrs (Add e1 (Var x)) = case Map.lookup x vrs of
                                    Just v -> Const (eval vrs (Add e1 (Var x)))
                                    Nothing -> Add (Var x) (Const (eval vrs e1))
  simplify vrs (Add (Var x) e1) = case Map.lookup x vrs of
                                    Just v -> Const (eval vrs (Add (Var x) e1))
                                    Nothing -> Add (Var x) (Const (eval vrs e1))
  simplify vrs (Add e1 e2) = Const (eval vrs (Add e1 e2))
  simplify vrs (Mult e1 (Var x)) = case Map.lookup x vrs of
                                    Just v -> Const (eval vrs (Mult e1 (Var x)))
                                    Nothing -> Mult (Var x) (Const (eval vrs e1))
  simplify vrs (Mult (Var x) e1) = case Map.lookup x vrs of
                                    Just v -> Const (eval vrs (Mult (Var x) e1))
                                    Nothing -> Mult (Var x) (Const (eval vrs e1))
  simplify vrs (Mult e1 e2) = Const (eval vrs (Mult e1 e2))
  linearization vrs e1 x y a b = Add (Add (simplify vrs e1) 
                                 (Mult (partDiff x e1) (simplify vrs (Add (Var x) (Neg (Var a)))))) 
                                 (Mult (partDiff y e1) (simplify vrs (Add (Var y) (Neg (Var b)))))
  multDiff ez ex ey x y t = Add (Mult (partDiff x ez) (partDiff t ex)) (Mult (partDiff y ez) (partDiff t ey))
  antiDeriv (Const a) = Mult (Const a) (Var "x")
  antiDeriv (Exp (Var x) (Const (-1.0))) = Ln (Var x)
  antiDeriv (Exp (Var x) (Const n)) = Mult (Exp (Var x) (Const (n+1))) (Exp (Const (n+1)) (Const (-1)))
  antiDeriv (NExp (Var x)) = (NExp (Var x))
  antiDeriv (NExp (Const a)) = Mult (NExp (Const a)) (Var "x")
  antiDeriv (Sin (Var x)) = Neg (Cos (Var x))
  antiDeriv (Cos (Var x)) = Sin (Var x)
  antiDeriv (Sin (Const a)) = Mult (Sin (Const a)) (Var "x")
  antiDeriv (Cos (Const a)) = Mult (Cos (Const a)) (Var "x"

