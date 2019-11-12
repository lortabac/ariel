{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ariel.Language.Expr where

import Ariel.Common.Types
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)

data Expr (ph :: Phase)
  = Int Integer
  | Float Double
  | String Text
  | Cons Tag (Expr ph)
  | Tuple (Vector (Expr ph))
  | At (Expr ph) TupleIx
  | Lam (Expr ph) (Expr ph)
  | App (Expr ph) (Expr ph)
  | Var Name
  | Case (Expr ph) (Map Tag (Expr ph))
  | Let (Expr ph) (Expr ph) (Expr ph)
  | Fix (Expr ph) (Expr ph)
  | IOPrim Name
  | Bind (Expr ph) (Expr ph)
  | Pure (Expr ph)
  deriving (Eq, Show)

infixl 9 `App`

infixl 1 `Bind`
