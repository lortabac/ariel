{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}

module Ariel.Runtime.Types where

import Ariel.Common.IOPrim
import Ariel.Common.Prim
import Ariel.Common.Types
import Ariel.Runtime.Env
import Data.Text (Text)
import GHC.Prim (Int#)

data PExpr
  = PConst Value
  | PGlobal Int
  | PVar Name
  | PAbs Name PExpr
  | PWildAbs Name PExpr
  | PApp PExpr PExpr
  | PPrim (Prim PExpr)
  | PIOPrim (IOPrim PExpr)
  | PIf PExpr PExpr PExpr
  | PBindIO PExpr PExpr
  | PFix PExpr
  deriving (Eq, Show)

data IExpr
  = IConst Value
  | IGlobal {-# UNPACK #-} Int
  | IVar ~Name {-# UNPACK #-} Int
  | IAbs ~Name IExpr
  | IDummyAbs ~Name IExpr
  | IApp IExpr IExpr
  | IPrim (Prim IExpr)
  | IIOPrim (IOPrim IExpr)
  | IIf IExpr ~IExpr ~IExpr
  | IBindIO IExpr IExpr
  | IFix IExpr
  deriving (Eq, Show)

data Value
  = VInt {-# UNPACK #-} Int#
  | VString {-# UNPACK #-} Text
  | VBool {-# UNPACK #-} Int#
  | VClos ~(Env Value) ~Name IExpr
  | VDummyClos ~(Env Value) ~Name IExpr
  | VIOPrim (IOPrim Value)
  | VBindIO Value Value
  deriving (Eq, Show)

boolToInt# :: Bool -> Int#
boolToInt# False = 0#
boolToInt# True = 1#
{-# INLINE boolToInt# #-}
