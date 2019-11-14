module Ariel.Hoas.Expr where

import Ariel.Common.Env
import Ariel.Common.Types
import Data.Text (Text)
import Data.Vector (Vector)

-- | An expression in the HOAS
data ExprH
  = IntH Integer
  | FloatH Double
  | StringH Text
  | ConsH ConsIx ExprH
  | TupleH (Vector ExprH)
  | AtH ExprH TupleIx
  | LamH ((Env ExprH -> ExprH -> ExprH) -> ExprH -> ExprH)
  | AppH ExprH ExprH
  | VarH Name
  | CaseH ExprH (Vector ExprH)
  | LetH ((Env ExprH -> ExprH -> ExprH) -> ExprH)
  | FixH ((Env ExprH -> ExprH -> ExprH) -> ExprH)
  | IOH (IO ExprH)
  | BindH ExprH ExprH
  | PureH ExprH

infixl 9 `AppH`

infixl 1 `BindH`
