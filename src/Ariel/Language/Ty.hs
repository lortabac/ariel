{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ariel.Language.Ty where

import Ariel.Common.Types
import Data.Vector (Vector)

data Ty (ph :: Phase)
  = TInt
  | TFloat
  | TString
  | TSum (Vector (Ty ph))
  | TTuple (Vector (Ty ph))
  | TArr (Ty ph) (Ty ph)
  deriving (Eq, Show)
