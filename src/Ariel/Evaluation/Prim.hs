{-# LANGUAGE OverloadedLists #-}

module Ariel.Evaluation.Prim where

import Ariel.Evaluation.Types
import Data.Text (Text)
import qualified Data.Text as Text

evalPrim :: Prim -> [Expr] -> Expr
evalPrim Eq [x, y] = if x == y then Cons 1 [] else Cons 0 []
evalPrim Plus [Int x, Int y] = Int (x + y)
evalPrim Minus [Int x, Int y] = Int (x - y)
evalPrim ShowInt [Int x] = Text (showt x)
evalPrim ConcatText [Text x, Text y] = Text (x <> y)
evalPrim p args = error ("Invalid Prim: " <> show (p, args))

showt :: Show a => a -> Text
showt = Text.pack . show
