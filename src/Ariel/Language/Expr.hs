{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ariel.Language.Expr where

import Ariel.Common.Types
import Data.Text (Text)
import Data.Vector (Vector)

-- | An expression in one of Ariel's phases
data Expr (ph :: Phase)
  = Int Integer
  | Float Double
  | String Text
  | Cons ConsIx (Expr ph)
  | Tuple (Vector (Expr ph))
  | At (Expr ph) TupleIx
  | Lam (Expr ph) (Expr ph)
  | App (Expr ph) (Expr ph)
  | Var Name
  | Case (Expr ph) (Vector (Expr ph))
  | Let (Expr ph) (Expr ph) (Expr ph)
  | Fix (Expr ph) (Expr ph)
  | IOPrim Name
  | Bind (Expr ph) (Expr ph)
  | Pure (Expr ph)
  deriving (Eq, Show)

infixl 9 `App`

infixl 1 `Bind`
