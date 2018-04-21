module ExprTest where

import           ExprDiff
import           ExprParser

import           ExprType

import qualified Data.Map.Strict as Map
import           Test.QuickCheck

sampleExpr1 :: Expr Int
sampleExpr1 = (var "x") !+ (var "y")


listToExpr1 :: [Double] -> Expr Double
listToExpr1 [x]    = Const x
listToExpr1 (x:xs) = Add (Const x) (listToExpr1 xs)
listToExpr1 []     = error "Not list to expression for empty"


test1 :: Int -> Bool
test1 x = eval (Map.fromList [("x",x),("y",-x)]) sampleExpr1 == 0

test1 :: Double -> Bool
test1 x = eval (Map.fromList [("x",x),("y",-x)]) sampleExpr1 == 0
--------------------------Squaring a number x-----------

sampleExpr2 :: Expr Double
sampleExpr2 = (var "x") !* (var "y")


test2 :: Double -> Bool
test2 x = eval (Map.fromList [("x",x),("y",x)]) sampleExpr2 == x^2
--------------------------------------------


sampleExpr3 :: Expr Double
sampleExpr3 = myCos (var "a")


test3 :: Double -> Bool
test3 a = eval (Map.fromList [("a",a)]) sampleExpr3 == cos a

------------------------------------------------

sampleExpr4 :: Expr Double
sampleExpr4 = mySin (var "b")


test4 :: Double -> Bool
test4 b = eval (Map.fromList [("b",b)]) sampleExpr4 == sin b
------------------------------------------------
