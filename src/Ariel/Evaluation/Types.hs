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
    extendEnvRec,
    lookupEnv,
    lookupEnvRec,
    setEnvIndex,
    extendRecEnv,
    lookupRecEnv,
  )
where

import Ariel.Syntax.Types
import Data.Coerce (coerce)
import Data.Sequence (Seq, (<|))
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

data Env' a = Env {_env :: Vector a, _recEnv :: Seq a}
  deriving (Eq, Show)

-- | Create an empty environment
emptyEnv :: Env
emptyEnv = Env V.empty Seq.empty

-- | Append an expression to the environment
extendEnv :: Expr -> Env -> Env
extendEnv e (Env env recEnv) = Env (V.cons e env) recEnv

-- | Append an expression to the environment
extendEnvRec :: Expr -> Env -> Env
extendEnvRec e (Env env recEnv) = Env (V.snoc env e) recEnv

-- | Lookup a variable in the environment
lookupEnv :: VarIx -> Env -> Expr
lookupEnv (VarIx i) (Env env _) = case env V.!? i of
  Just e -> e
  Nothing -> error ("Out of bounds variable: " <> show i)

-- | Lookup a variable in the environment
lookupEnvRec :: Env -> Expr
lookupEnvRec (Env env _) = case env V.!? (V.length env - 1) of
  Just e -> e
  Nothing -> error ("Out of bounds variable: " <> show (V.length env - 1))

setEnvIndex :: VarIx -> Expr -> Env -> Env
setEnvIndex (VarIx i) e (Env env recEnv) = Env (V.modify (\v -> MV.write v i e) env) recEnv

-- | Append an expression to the recursion environment
extendRecEnv :: Expr -> Env -> Env
extendRecEnv e (Env env recEnv) = Env env (e <| recEnv)

-- | Lookup a variable in the recursion environment
lookupRecEnv :: VarIx -> Env -> Expr
lookupRecEnv (VarIx i) (Env _ recEnv) = case Seq.lookup i recEnv of
  Just e -> e
  Nothing -> error ("Out of bounds recursive variable: " <> show i)
