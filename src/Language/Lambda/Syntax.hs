{-# LANGUAGE GADTs #-}
module Language.Lambda.Syntax where

import Data.Maybe (fromMaybe)

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

eval :: Context -> T -> V
eval c (Var name) = fromMaybe (Abs' name (Var name)) (fmap (eval c) (lookup' name c))
eval c (App (Abs s t) t2) = eval (insert' s t2 c) t
eval c (Abs name t) = eval c t

