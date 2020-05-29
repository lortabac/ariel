{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Ariel.Evaluation.Types
  ( Expr (..),
    Prim (..),
    readPrim,
    IOPrim (..),
    readIOPrim,
    Env,
    emptyEnv,
    extendEnv,
    lookupEnv,
    extendRecEnv,
    lookupRecEnv,
  )
where

import Ariel.Syntax.Types
import Data.Coerce (coerce)
import Data.Sequence ((<|), Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

-- | Nameless core expression
data Expr
  = Int Integer
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
  | InEnv Env Expr
  | Case Expr (Vector Expr)
  | Let Name Expr Expr
  | LetRec Name Expr Expr
  | Prim Prim [Expr]
  | IOPrim IOPrim [Expr]
  | Bind Expr Expr
  | Pure Expr
  deriving (Eq, Show, Generic)

-- | Primitive pure operation
data Prim
  = Eq
  | Plus
  | Minus
  | ConcatText
  deriving (Eq, Read, Show)

readPrim :: Name -> Prim
readPrim = read . coerce

-- | Primitive IO operation
data IOPrim
  = WriteLn
  | ReadLine
  deriving (Eq, Read, Show)

readIOPrim :: Name -> IOPrim
readIOPrim = read . coerce

-- | Evaluation environment
type Env = Env' Expr

data Env' a = Env {_env :: (Seq a), _recEnv :: (Seq a)}
  deriving (Eq, Show)

-- | Create an empty environment
emptyEnv :: Env
emptyEnv = Env Seq.empty Seq.empty

-- | Append an expression to the environment
extendEnv :: Expr -> Env -> Env
extendEnv e (Env env recEnv) = Env (e <| env) recEnv

-- | Lookup a variable in the environment
lookupEnv :: VarIx -> Env -> Expr
lookupEnv (VarIx i) (Env env _) = case Seq.lookup i env of
  Just e -> e
  Nothing -> error ("Out of bounds variable: " <> show i)

-- | Append an expression to the recursion environment
extendRecEnv :: Expr -> Env -> Env
extendRecEnv e (Env env recEnv) = Env env (e <| recEnv)

-- | Lookup a variable in the recursion environment
lookupRecEnv :: VarIx -> Env -> Expr
lookupRecEnv (VarIx i) (Env _ recEnv) = case Seq.lookup i recEnv of
  Just e -> e
  Nothing -> error ("Out of bounds recursive variable: " <> show i)
