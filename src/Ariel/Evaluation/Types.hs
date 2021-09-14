{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Ariel.Evaluation.Types
  ( Expr (..),
    Prim1 (..),
    Prim2 (..),
    readPrim1,
    readPrim2,
    IOPrim (..),
    readIOPrim,
    Env,
    emptyEnv,
    extendEnv,
    lookupEnv,
    setEnvHead,
  )
where

import Ariel.Syntax.Types
import Data.Coerce (coerce)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import GHC.Generics (Generic)

-- | Nameless core expression
data Expr
  = Int {-# UNPACK #-} Int
  | Double Double
  | Text Text
  | Cons ConsIx [Expr]
  | Tuple (Vector Expr)
  | Get TupleIx Expr
  | Update TupleIx Expr Expr
  | Abs Name Expr
  | Clos Name Expr Env
  | App Expr Expr
  | Var Name VarIx
  | RecVar Name VarIx
  | Case Expr (Vector Expr)
  | Let Name Expr Expr
  | LetRec Name Expr Expr
  | RecClos Expr Env
  | Prim1 Prim1 Expr
  | Prim2 Prim2 Expr Expr
  | IOPrim IOPrim [Expr]
  | Bind Expr Expr
  | Pure Expr
  | Undefined
  deriving (Eq, Show, Generic)

-- | Primitive pure unary operation
data Prim1
  = ShowInt
  deriving (Eq, Read, Show)

-- | Primitive pure binary operation
data Prim2
  = Eq
  | Plus
  | Minus
  | ConcatText
  deriving (Eq, Read, Show)

readPrim1 :: Name -> Prim1
readPrim1 = read . coerce

readPrim2 :: Name -> Prim2
readPrim2 = read . coerce

-- | Primitive IO operation
data IOPrim
  = WriteLn
  | ReadLine
  deriving (Eq, Read, Show)

readIOPrim :: Name -> IOPrim
readIOPrim = read . coerce

-- | Evaluation environment
type Env = Env' Expr

newtype Env' a = Env (Seq a)
  deriving (Eq, Show)

-- | Create an empty environment
emptyEnv :: Env
emptyEnv = Env Seq.empty

-- | Append an expression to the environment
extendEnv :: Expr -> Env -> Env
extendEnv e (Env env) = Env (e <| env)

-- | Lookup a variable in the environment
lookupEnv :: VarIx -> Env -> Expr
lookupEnv (VarIx i) (Env env) = case Seq.lookup i env of
  Just e -> e
  Nothing -> error ("Out of bounds variable: " <> show i)

setEnvHead :: Expr -> Env -> Env
setEnvHead e (Env (_ :<| env)) = Env (e <| env)
