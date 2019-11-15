{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ariel.Language.Expr where

import Ariel.Common.Types
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)

-- | An expression in one of Ariel's phases
data Expr (ph :: Phase)
  = Int Integer
  | Float Double
  | String Text
  | CoreConsC (CoreOnlyS ph CoreCons)
  | ConsC (UntilS 'Desugared ph (Cons ph))
  | Tuple (Vector (Expr ph))
  | At (Expr ph) TupleIx
  | Lam (Expr ph) (Expr ph)
  | App (Expr ph) (Expr ph)
  | Var Name
  | CoreCaseC (CoreOnlyS ph CoreCase)
  | CaseC (UntilS 'Desugared ph (Case ph))
  | Let (Expr ph) (Expr ph) (Expr ph)
  | Fix (Expr ph) (Expr ph)
  | IOPrim Name
  | Bind (Expr ph) (Expr ph)
  | Pure (Expr ph)

infixl 9 `App`

infixl 1 `Bind`

deriving instance
  ( Eq (CoreOnlyS ph CoreCons),
    Eq (UntilS 'Desugared ph (Cons ph)),
    Eq (CoreOnlyS ph CoreCase),
    Eq (UntilS 'Desugared ph (Case ph))
  ) =>
  Eq (Expr ph)

deriving instance
  ( Show (CoreOnlyS ph CoreCons),
    Show (UntilS 'Desugared ph (Cons ph)),
    Show (CoreOnlyS ph CoreCase),
    Show (UntilS 'Desugared ph (Case ph))
  ) =>
  Show (Expr ph)

data CoreCons = CoreCons ConsIx (Expr 'Core) deriving (Eq, Show)

data Cons (ph :: Phase) = Cons Tag (Expr ph)

deriving instance Eq (Cons 'Full)

deriving instance Eq (Cons 'Desugared)

deriving instance Show (Cons 'Full)

deriving instance Show (Cons 'Desugared)

data CoreCase = CoreCase (Expr 'Core) (Vector (Expr 'Core)) deriving (Eq, Show)

data Case ph = Case (Expr ph) (Map Tag (Expr ph))

deriving instance Eq (Case 'Full)

deriving instance Eq (Case 'Desugared)

deriving instance Show (Case 'Full)

deriving instance Show (Case 'Desugared)
