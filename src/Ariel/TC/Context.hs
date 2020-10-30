{-# LANGUAGE TemplateHaskell #-}

module Ariel.TC.Context
  ( Ctx (..),
    emptyCtx,
    LocalCtx,
    localCtx,
    globalCtx,
    emptyLocalCtx,
    extendLocalCtx,
    lookupLocalCtx,
    localCtxElems,
    GlobalCtx,
    emptyGlobalCtx,
    extendGlobalCtx,
    lookupGlobalCtx,
  )
where

import Ariel.Syntax.Types
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lens.Micro.TH (makeLenses)

data Ctx a = Ctx {_localCtx :: LocalCtx a, _globalCtx :: GlobalCtx a}
  deriving (Eq, Show)

emptyCtx :: Ctx a
emptyCtx = Ctx {_localCtx = emptyLocalCtx, _globalCtx = emptyGlobalCtx}

newtype LocalCtx a = LocalCtx (Map Name a) deriving (Eq, Show)

emptyLocalCtx :: LocalCtx a
emptyLocalCtx = LocalCtx Map.empty

extendLocalCtx :: Name -> a -> LocalCtx a -> LocalCtx a
extendLocalCtx k v (LocalCtx ctx) = LocalCtx (Map.insert k v ctx)

lookupLocalCtx :: Name -> LocalCtx a -> Maybe a
lookupLocalCtx k (LocalCtx ctx) = Map.lookup k ctx

localCtxElems :: LocalCtx a -> [a]
localCtxElems (LocalCtx ctx) = Map.elems ctx

newtype GlobalCtx a = GlobalCtx (Map GName a) deriving (Eq, Show)

emptyGlobalCtx :: GlobalCtx a
emptyGlobalCtx = GlobalCtx Map.empty

extendGlobalCtx :: GName -> a -> GlobalCtx a -> GlobalCtx a
extendGlobalCtx k v (GlobalCtx ctx) = GlobalCtx (Map.insert k v ctx)

lookupGlobalCtx :: GName -> GlobalCtx a -> Maybe a
lookupGlobalCtx k (GlobalCtx ctx) = Map.lookup k ctx

makeLenses ''Ctx
