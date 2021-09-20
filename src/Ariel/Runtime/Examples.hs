module Ariel.Runtime.Examples where

import Ariel.Runtime.Eval
import Ariel.Runtime.Prim
import Ariel.Runtime.Types

addition :: PExpr
addition = PApp (PApp (PApp plus3 (PConst (VInt 1))) (PConst (VInt 2))) (PConst (VInt 3))

evalExample :: PExpr -> String
evalExample = showValue . evalPExpr mempty

showValue :: Value -> String
showValue (VInt i) = show i
showValue (VString s) = show s
showValue _ = "<>"
