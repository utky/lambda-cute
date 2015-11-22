{-# LANGUAGE GADTs #-}

import Prelude hiding (True, False)

data T where
  True :: T
  False :: T
  If :: T -> T -> T -> T
  O :: T
  Succ :: T -> T
  Pred :: T -> T
  IsO :: T -> T

data V where
  O :: V
  Succ :: V -> V

data Eval where
  ESucc :: (a -> b) -> T a -> T b
  EPred :: 

eval :: T -> T
eval (Succ t) = Succ t'
