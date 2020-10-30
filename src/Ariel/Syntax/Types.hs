{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ariel.Syntax.Types where

import Data.String (IsString)

-- | Variant tag
newtype Tag = Tag {unTag :: String} deriving (Eq, Ord, Show, IsString)

-- | Field label
newtype Label = Label {unLabel :: String} deriving (Eq, Ord, Show, IsString)

-- | Variable name
newtype Name = Name {unName :: String} deriving (Eq, Ord, Show, IsString)

-- | Module name
newtype ModuleName = ModuleName {unModuleName :: [String]}
  deriving (Eq, Ord, Show)

-- | Qualified identifier (constant, function or data type)
data QName = QName {qnameModule :: ModuleName, qnameName :: Name}
  deriving (Eq, Ord, Show)

-- | Qualified tag (it includes the type the constructor belongs to)
data QTag = QTag {qtagType :: QName, qtagTag :: Tag}
  deriving (Eq, Ord, Show)

-- | Qualified label (it includes the type the field belongs to)
data QLabel = QLabel {qlabelType :: QName, qlabelLabel :: Label}
  deriving (Eq, Ord, Show)

-- | Global name
data GName
  = GName QName
  | GTag QTag
  | GLabel QLabel
  deriving (Eq, Ord, Show)

-- | Type variable
newtype TyVar = TyVar {unTyVar :: String} deriving (Eq, Ord, Show, IsString)

tyVarSupply :: [TyVar]
tyVarSupply = go (TyVar "a")
  where
    go tv = tv : go (nextTyVar tv)

nextTyVar :: TyVar -> TyVar
nextTyVar (TyVar "z") = TyVar "z1"
nextTyVar (TyVar [c]) = TyVar ([succ c])
nextTyVar (TyVar ('z':i)) = TyVar ('z' : show (read i + 1 :: Int))
nextTyVar (TyVar tv) = error ("Invalid nextTyVar: " <> tv)
