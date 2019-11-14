{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ariel.Language.Expr where

import Ariel.Common.Types
import Data.Text (Text)
import Data.Vector (Vector)

-- | An expression in one of Ariel's phases
data Expr (ph :: Phase)
  = Int Integer
  | Float Double
  | String Text
  | ConsC (CoreOnlyS ph Cons)
  | VariantC (UntilS 'Desugared ph (Variant ph))
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

infixl 9 `App`

infixl 1 `Bind`

deriving instance (Eq (CoreOnlyS ph Cons), Eq (UntilS 'Desugared ph (Variant ph))) => Eq (Expr ph)

deriving instance (Show (CoreOnlyS ph Cons), Show (UntilS 'Desugared ph (Variant ph))) => Show (Expr ph)

data Cons = Cons ConsIx (Expr 'Core) deriving (Eq, Show)

data Variant (ph :: Phase) = Variant Tag (Expr ph)

deriving instance Eq (Variant 'Full)

deriving instance Eq (Variant 'Desugared)

deriving instance Show (Variant 'Full)

deriving instance Show (Variant 'Desugared)
