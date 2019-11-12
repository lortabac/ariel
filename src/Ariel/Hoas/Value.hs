{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}

module Ariel.Hoas.Value where

import Ariel.Hoas.Expr
import Data.Text (Text)

isVal :: ExprH -> Bool
isVal IntH {} = True
isVal FloatH {} = True
isVal StringH {} = True
isVal (ConsH _ e) = isVal e
isVal (TupleH xs) = and (isVal <$> xs)
isVal (AtH e _) = isVal e
isVal LamH {} = True
isVal (AppH _ _) = False
isVal VarH {} = False
isVal CaseH {} = False
isVal LetH {} = False
isVal FixH {} = False
isVal IOH {} = True
isVal (BindH f g) = isVal f && isVal g
isVal (PureH e) = isVal e

class IsValue a where
  getValue :: ExprH -> Maybe a

instance IsValue Integer where
  getValue (IntH i) = Just i
  getValue _ = Nothing

instance IsValue Text where
  getValue (StringH s) = Just s
  getValue _ = Nothing

instance IsValue Double where
  getValue (FloatH f) = Just f
  getValue _ = Nothing

instance (IsValue t1, IsValue t2) => IsValue (t1, t2) where
  getValue (TupleH [x1, x2]) = (,) <$> getValue x1 <*> getValue x2
  getValue _ = Nothing

instance (IsValue t1, IsValue t2, IsValue t3) => IsValue (t1, t2, t3) where
  getValue (TupleH [x1, x2, x3]) = (,,) <$> getValue x1 <*> getValue x2 <*> getValue x3
  getValue _ = Nothing
