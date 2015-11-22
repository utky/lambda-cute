{-# LANGUAGE GADTs #-}
module Language.Lambda.Syntax where

import Data.Maybe (fromMaybe)

{- | Lambda syntax
 - t ::=
 -   x
 -   λx. t
 -   x x
 -}
data T where
  Var :: String -> T
  Abs :: String -> T -> T
  App :: T -> T -> T
  deriving (Show)

data V = Abs' String T deriving (Show)

type Context = [(String, T)]

empty' :: Context
empty' = []

insert' :: String -> T -> Context -> Context
insert' s t c = (s, t) : c

lookup' :: String -> Context -> Maybe T
lookup' s [] = Nothing
lookup' s ((s', t'):cs) 
  | s == s' = Just t'
  | otherwise = lookup' s cs

reduce :: Context -> T -> T
reduce c (Var v)               = fromMaybe (Var v) (lookup v c)
reduce c (App t1 t2@(App _ _)) = App t1 (reduce c t2) -- call by value
reduce c (App (Abs v t1) t2)   = reduce (insert' v t2 c) t1 -- beta reduction
reduce c t = t

eval :: Context -> T -> V
eval c (Var v) = Abs' v (Var v)
eval c (Abs v t) = Abs' v t
eval c t = eval c (reduce c t)

