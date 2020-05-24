{-# LANGUAGE DeriveGeneric #-}

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
    envToList,
    printEnv,
    extendRecEnv,
    lookupRecEnv,
  )
where

import Ariel.Syntax.Types
import Control.Monad ((<=<))
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Vector (Vector)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as V
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
  | Abs Expr
  | App Expr Expr
  | Var VarIx
  | RecVar VarIx
  | Rec Env Expr
  | Case Expr (Vector Expr)
  | Let Expr Expr
  | LetRec Expr Expr
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

-- | The index of the last-added element
type Offset = Int

-- | The index of the last-added recursive element
type RecIx = Int

-- The environment is implemented as a big mutable vector.
-- It is extended to the left, starting from the last index of the vector.
-- An offset is kept, to keep track of the index of the last-added element.
-- The vector also contains a 'recursion environment',
-- that is all the letrec expressions.
-- The recursion environment is extended to the right, starting from the index 0.
-- A recursion offset is kept, to keep track of the index of the last-added element.
data Env' a = Env !Offset !RecIx !(IOVector a)

-- Dummy Eq instance
instance Eq (Env' a) where
  _ == _ = False

-- Dummy Show instance
instance Show (Env' a) where
  show _ = "{Env}"

-- | The size of the environment
envSize :: Int
envSize = 10000000

-- | Create an empty environment
emptyEnv :: IO Env
emptyEnv = do
  env <- V.new envSize
  pure $ Env envSize 0 env

-- | Append an expression to the environment
extendEnv :: Expr -> Env -> IO Env
extendEnv e (Env offset recI env)
  | offset > recI = Env offset' recI env <$ V.write env offset' e
  | otherwise = error "No more space in the environment"
  where
    offset' = offset - 1

-- | Lookup a variable in the environment
lookupEnv :: VarIx -> Env -> IO Expr
lookupEnv (VarIx i) (Env offset _ env) = V.read env (i + offset)

-- | Build a list from the relevant slice of the environment
--   For debugging purposes
envToList :: Env -> IO [Expr]
envToList (Env offset _ env) = for [offset .. (envSize - 1)] (V.read env)

-- | Print the list of the relevant slice of the environment
--   For debugging purposes
printEnv :: Env -> IO ()
printEnv = print . zip ([0 ..] :: [Int]) <=< envToList

-- | Append an expression to the recursion environment
extendRecEnv :: Expr -> Env -> IO Env
extendRecEnv e (Env offset recI env) = Env offset (recI + 1) env <$ V.write env recI e

-- | Lookup a variable from the recursion environment
lookupRecEnv :: VarIx -> Env -> IO Expr
lookupRecEnv (VarIx i) (Env _ _ env) = V.read env i
