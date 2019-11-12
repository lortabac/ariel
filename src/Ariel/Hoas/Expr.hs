module Ariel.Hoas.Expr where

import Ariel.Common.Env
import Ariel.Common.Types
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)

data ExprH
  = IntH Integer
  | FloatH Double
  | StringH Text
  | ConsH Tag ExprH
  | TupleH (Vector ExprH)
  | AtH ExprH TupleIx
  | LamH ((Env ExprH -> ExprH -> ExprH) -> ExprH -> ExprH)
  | AppH ExprH ExprH
  | VarH Name
  | CaseH ExprH (Map Tag ExprH)
  | LetH ((Env ExprH -> ExprH -> ExprH) -> ExprH)
  | FixH ((Env ExprH -> ExprH -> ExprH) -> ExprH)
  | IOH (IO ExprH)
  | BindH ExprH ExprH
  | PureH ExprH

infixl 9 `AppH`

infixl 1 `BindH`
