{-# LANGUAGE GADTs #-}
module Language.Lambda.Syntax where

import Data.Maybe (fromMaybe)

{- | Lambda syntax
 - t ::=
 -   x
 -   λx. t
 -   x x
 -}
data Term where
  Var :: String -> Term
  Abs :: String -> Term -> Term
  App :: Term -> Term -> Term
  deriving (Show)

data Val = Abs' String Term deriving (Show)

type Context = [(String, Term)]

empty' :: Context
empty' = []

insert' :: String -> Term -> Context -> Context
insert' s t c = (s, t) : c

lookup' :: String -> Context -> Maybe Term
lookup' s [] = Nothing
lookup' s ((s', t'):cs) 
  | s == s' = Just t'
  | otherwise = lookup' s cs

reduce :: Context -> Term -> Term
reduce c (Var v)               = fromMaybe (Var v) (lookup v c)
reduce c (App t1 t2@(App _ _)) = App t1 (reduce c t2) -- call by value
reduce c (App (Abs v t1) t2)   = reduce (insert' v t2 c) t1 -- beta reduction
reduce c t = t

eval :: Context -> Term -> Val
eval c (Var v) = Abs' v (Var v)
eval c (Abs v t) = Abs' v t
eval c t = eval c (reduce c t)

