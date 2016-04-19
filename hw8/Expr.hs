module Expr where

data Expr = Plus Expr Expr | Mul Expr Expr | Pow Expr Expr | Limit | Ord Integer
instance Show Expr where
  show (Pow l@Limit r) = show l ++ "^(" ++ show r ++ ")"
  show (Pow l@(Ord _) r) = show l ++ "^(" ++ show r ++ ")"
  show (Pow l r) = "(" ++ show l ++ ")^(" ++ show r ++ ")"
  show (Mul l r) = pprint l ++ "*" ++ pprint r
  show (Plus l r) = show l ++ "+" ++ show r
  show Limit = "W"
  show (Ord i) = show i

pprint :: Expr -> String
pprint (Plus l r) = "(" ++ show l ++ "+" ++ show r ++ ")"
pprint e@_        = show e
