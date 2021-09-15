{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}

module Ariel.Evaluation.Prim where

import Ariel.Evaluation.Types
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Exts

evalPrim1 :: Prim1 -> Expr -> Expr
evalPrim1 ShowInt (Int x) = Text (showt (Int x))
evalPrim1 p e = error ("Invalid Prim1: " <> show (p, e))
{-# INLINE evalPrim1 #-}

evalPrim2 :: Prim2 -> Expr -> Expr -> Expr
evalPrim2 Eq (Int x) (Int y) = if eqInt x y then Cons 1 [] else Cons 0 []
evalPrim2 Plus (Int x) (Int y) = Int (x +# y)
evalPrim2 Minus (Int x) (Int y) = Int (x -# y)
evalPrim2 ConcatText (Text x) (Text y) = Text (x <> y)
evalPrim2 p e1 e2 = error ("Invalid Prim2: " <> show (p, e1, e2))
{-# INLINE evalPrim2 #-}

showt :: Show a => a -> Text
showt = Text.pack . show

eqInt :: Int# -> Int# -> Bool
eqInt x y = isTrue# (x ==# y)
