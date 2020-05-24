module Ariel.Evaluation.Env
  ( Env,
    emptyEnv,
    extendEnv,
    lookupEnv,
    updateEnvHead,
    envToList,
    printEnv,
  )
where

import Ariel.Common.Types (Expr (..))
import Ariel.Syntax.Types (VarIx (..))
import Control.Monad ((<=<))
import Data.Traversable (for)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as V

type Env = Env' Expr

data Env' a = Env !Int !(IOVector a)

envSize :: Int
envSize = 10000000

emptyEnv :: IO Env
emptyEnv = Env envSize <$> V.new envSize

extendEnv :: Expr -> Env -> IO Env
extendEnv e (Env offset env) = Env offset' env <$ V.write env offset' e
  where
    offset' = offset - 1

lookupEnv :: VarIx -> Env -> IO Expr
lookupEnv (VarIx i) (Env offset env) = V.read env (i + offset)

updateEnvHead :: Expr -> Env -> IO Env
updateEnvHead e (Env offset env) = Env offset env <$ V.write env offset e

envToList :: Env -> IO [Expr]
envToList (Env offset env) = for [offset .. (envSize - 1)] (V.read env)

printEnv :: Env -> IO ()
printEnv = print . zip ([0 ..] :: [Int]) <=< envToList
