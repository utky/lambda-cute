{-# LANGUAGE GADTs #-}
module Language.Bool.Syntax where



data TBool where
  TTrue :: TBool
  TFalse :: TBool
  TIf :: TBool -> TBool -> TBool -> TBool

eval :: TBool -> Bool
eval TTrue  = True
eval TFalse = False
eval (TIf TTrue  thenT _) = eval thenT
eval (TIf TFalse _ elseT) = eval elseT
eval (TIf condT thenT elseT) =
    let t = if eval condT then thenT else elseT
    in eval t

