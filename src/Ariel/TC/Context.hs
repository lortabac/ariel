module Ariel.TC.Context
  ( Ctx,
    emptyCtx,
    extendCtx,
    lookupCtx,
  )
where

import Ariel.Syntax.Types
import Data.Map (Map)
import qualified Data.Map as Map

newtype Ctx a = Ctx (Map Name a) deriving (Eq, Show)

emptyCtx :: Ctx a
emptyCtx = Ctx Map.empty

extendCtx :: Name -> a -> Ctx a -> Ctx a
extendCtx k v (Ctx ctx) = Ctx (Map.insert k v ctx)

lookupCtx :: Name -> Ctx a -> Maybe a
lookupCtx k (Ctx ctx) = Map.lookup k ctx
